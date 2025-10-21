open Ast

let fresh_var =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "$x" ^ string_of_int !counter

let rec free_vars = function
  | Var y -> [ y ]
  | Abs (y, e) -> List.filter (fun x -> x <> y) (free_vars e)
  | App (e1, e2) -> free_vars e1 @ free_vars e2
  | Let (y, e1, e2) ->
    free_vars e1 @ List.filter (fun x -> x <> y) (free_vars e2)
  | If (e1, e2, e3) -> free_vars e1 @ free_vars e2 @ free_vars e3
  | Seq (e1, e2) -> free_vars e1 @ free_vars e2
  | IsZero e | Succ e | Pred e | Print e -> free_vars e
  | Tuple l -> List.concat (List.map free_vars l)
  | _ -> []

(* alpha-conversion *)
let rec rename x s = function
  | Var y when y = x -> Var s
  | Abs (y, e) -> Abs ((if y = x then s else y), rename x s e)
  | App (e1, e2) -> App (rename x s e1, rename x s e2)
  | Let (y, e1, e2) ->
    Let ((if y = x then s else y), rename x s e1, rename x s e2)
  | If (e1, e2, e3) -> If (rename x s e1, rename x s e2, rename x s e3)
  | Seq (e1, e2) -> Seq (rename x s e1, rename x s e2)
  | IsZero e -> IsZero (rename x s e)
  | Succ e -> Succ (rename x s e)
  | Pred e -> Pred (rename x s e)
  | Print e -> Print (rename x s e)
  | Tuple l -> Tuple (List.map (rename x s) l)
  | e -> e

let rec subst x s = function
  | Var y when y = x -> s
  | Var y -> Var y
  | Abs (y, e) when y = x -> Abs (y, e) (* shadowed, stop *)
  | Abs (y, e) when List.mem y (free_vars s) ->
    let fresh = fresh_var () in
    Abs (fresh, subst x s (rename y fresh e))
    (* avoid capture *)
  | Abs (y, e) -> Abs (y, subst x s e)
  | App (e1, e2) -> App (subst x s e1, subst x s e2)
  | Let (y, e1, e2) when y = x ->
    Let (y, subst x s e1, e2) (* shadowed, don't subst in e2 *)
  | Let (y, e1, e2) -> Let (y, subst x s e1, subst x s e2)
  | If (e1, e2, e3) -> If (subst x s e1, subst x s e2, subst x s e3)
  | Seq (e1, e2) -> Seq (subst x s e1, subst x s e2)
  | IsZero e -> IsZero (subst x s e)
  | Succ e -> Succ (subst x s e)
  | Pred e -> Pred (subst x s e)
  | Print e -> Print (subst x s e)
  | Tuple l -> Tuple (List.map (subst x s) l)
  | e -> e

let is_value = function Var _ | Abs _ | Val _ -> true | _ -> false

let rec exec_file env ic =
  let lexbuf = Lexing.from_channel ic in
  let stmts = Parser.main Lexer.main lexbuf in
  let env' = exec_stmts [] stmts in
  close_in ic;
  env' @ env

and exec_stmts env stmts =
  match stmts with
  | [] -> env
  | stmt :: rest ->
    let env' = exec_stmt env stmt in
    exec_stmts env' rest

and exec_stmt env = function
  | Assign (x, e) -> (x, e) :: env
  | Include filename -> exec_file env (open_in filename)
  | Eval e ->
    print_endline ("▶ " ^ Ast.string_of_expr e ^ ";;");
    let e' = eval env e in
    print_endline (Ast.string_of_expr e');
    env
  | Step e ->
    print_endline ("▸▸ " ^ Ast.string_of_expr e ^ ";;");
    let rec step_loop expr =
      let expr' = step env expr in
      if expr' = expr then expr'
      else (
        print_endline ("▸▸ " ^ Ast.string_of_expr expr');
        step_loop expr')
    in
    let _ = step_loop e in
    env

and eval env e =
  let e' = step env e in
  if e' <> e then eval env e' else e

and step env = function
  | Var x when List.mem_assoc x env -> List.assoc x env
  | App (Tuple l1, e2) -> Tuple (l1 @ [ e2 ])
  | App (Abs (x1, e1), _) when not (List.mem x1 (free_vars e1)) ->
    e1 (* eta-reduction *)
  | App (Abs (x1, e1), v2) when is_value v2 ->
    subst x1 v2 e1 (* beta-reduction *)
  | App (Var x1, v2) when is_value v2 && List.mem_assoc x1 env ->
    App (List.assoc x1 env, v2)
  | App (e1, v2) when is_value v2 -> App (step env e1, v2)
  | App (Abs (x1, e1), e2) -> App (Abs (x1, e1), step env e2)
  | App (e1, Var x2) when List.mem_assoc x2 env -> App (e1, List.assoc x2 env)
  | App (e1, e2) ->
    let e1' = step env e1 in
    if e1' <> e1 then App (e1', e2) else App (e1, step env e2)
  | Let (x, v1, e2) when is_value v1 -> App (Abs (x, e2), v1)
  | Let (x, e1, e2) -> Let (x, step env e1, e2)
  | If (e1, e2, e3) -> (
    match e1 with
    | Val (Bool true) -> e2
    | Val (Bool false) -> e3
    | v when is_value v -> Wrong
    | _ -> If (step env e1, e2, e3))
  | Seq (v1, e2) when is_value v1 -> e2
  | Seq (e1, e2) ->
    let e1' = step env e1 in
    if e1' <> e1 then Seq (e1', e2) else Seq (e1, step env e2)
  | IsZero e -> (
    match e with
    | Val (Num 0) -> Val (Bool true)
    | Val (Num _) -> Val (Bool false)
    | v when is_value v -> Wrong
    | _ -> IsZero (step env e))
  | Succ e -> (
    match e with
    | Val (Num n) -> Val (Num (n + 1))
    | v when is_value v -> Wrong
    | _ -> Succ (step env e))
  | Pred e -> (
    match e with
    | Val (Num 0) -> Val (Num 0)
    | Val (Num n) -> Val (Num (n - 1))
    | v when is_value v -> Wrong
    | _ -> Pred (step env e))
  | Print e ->
    let e' = step env e in
    if e' <> e then Print e'
    else (
      print_endline (Ast.string_of_expr (step env e));
      Val Unit)
  | Tuple l -> Tuple (List.map (step env) l)
  | e -> e
