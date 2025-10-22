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
  | IsZero e | Succ e | Pred e | Print (_, e) -> free_vars e
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
  | Print (nl, e) -> Print (nl, rename x s e)
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
  | Print (nl, e) -> Print (nl, subst x s e)
  | Tuple l -> Tuple (List.map (subst x s) l)
  | e -> e

let is_value = function Var _ | Abs _ | Val _ -> true | _ -> false

let rec exec_file env filename ic =
  (* Read all input first for error reporting *)
  let all_content =
    let buf = Buffer.create 1024 in
    try
      while true do
        Buffer.add_channel buf ic 1024
      done;
      Buffer.contents buf
    with End_of_file -> Buffer.contents buf
  in
  let all_lines = String.split_on_char '\n' all_content in
  let lexbuf = Lexing.from_string all_content in
  (* Set the filename for proper position tracking *)
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with pos_fname = filename };
  try
    let stmts = Parser.main Lexer.main lexbuf in
    let env' = exec_stmts [] stmts in
    close_in ic;
    env' @ env
  with Parser.Error ->
    let pos = lexbuf.Lexing.lex_start_p in
    let line = pos.Lexing.pos_lnum in
    let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
    let token = Lexing.lexeme lexbuf in
    close_in ic;
    let is_eof = token = "" || token = "\000" in
    let error_context =
      if is_eof then
        (* For EOF errors, show the last non-empty line *)
        let rec find_last_nonempty idx =
          if idx < 0 then None
          else
            let line_content = List.nth all_lines idx in
            if String.trim line_content <> "" then Some (idx + 1, line_content)
            else find_last_nonempty (idx - 1)
        in
        match find_last_nonempty (List.length all_lines - 1) with
        | Some (_, content) ->
          Printf.sprintf "\n  %s\n  %s^ (after this)" content
            (String.make (String.length content) ' ')
        | None -> ""
      else if line > 0 && line <= List.length all_lines then
        let line_content = List.nth all_lines (line - 1) in
        Printf.sprintf "\n  %s\n  %s^" line_content
          (String.make (max 0 (col - 1)) ' ')
      else ""
    in
    if is_eof then
      failwith
        (Printf.sprintf "Syntax error at %s:%d:%d: unexpected end of input%s"
           filename line col error_context)
    else
      failwith
        (Printf.sprintf "Syntax error at %s:%d:%d: unexpected '%s'%s" filename
           line col token error_context)

and exec_stmts env stmts =
  match stmts with
  | [] -> env
  | stmt :: rest ->
    let env' = exec_stmt env stmt in
    exec_stmts env' rest

and exec_stmt env = function
  | Assign (x, e) -> (x, e) :: env
  | Include filename -> exec_file env filename (open_in filename)
  | Eval e ->
    print_endline ("▶ " ^ Ast.string_of_expr false e);
    let e' = eval env e in
    print_endline (Ast.string_of_expr false e');
    env
  | Step e ->
    print_endline ("▸▸ " ^ Ast.string_of_expr false e);
    let rec step_loop expr =
      let expr' = step env expr in
      if expr' = expr then expr'
      else (
        print_endline ("▸▸ " ^ Ast.string_of_expr false expr');
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
  | Print (nl, e) ->
    let e' = step env e in
    if e' <> e then Print (nl, e')
    else (
      (if nl then print_endline else print_string)
        (Ast.string_of_expr true (step env e));
      Val Unit)
  | Tuple l -> Tuple (List.map (step env) l)
  | e -> e
