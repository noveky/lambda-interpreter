type stmt =
  | Assign of string * expr
  | Include of string
  | Eval of expr
  | Step of expr

and expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Seq of expr * expr
  | Val of value
  | If of expr * expr * expr
  | IsZero of expr
  | Succ of expr
  | Pred of expr
  | Print of expr
  | Tuple of expr list
  | Wrong

and value =
  | Num of int
  | Bool of bool
  | Str of string
  | List of value list
  | Unit

let precedence = function
  | Var _ | Val _ | Wrong -> 6
  | App _ | IsZero _ | Succ _ | Pred _ | Print _ | Tuple _ -> 5
  | Abs _ -> 4
  | Seq _ -> 3
  | If _ -> 2
  | Let _ -> 1

let is_right_associative = function Abs _ -> true | _ -> false

let rec string_of_expr_prec parent_prec expr =
  let current_prec = precedence expr in
  let needs_parens = current_prec < parent_prec in
  let result =
    match expr with
    | Var x -> x
    | Val v -> string_of_value v
    | Wrong -> "wrong"
    | Abs (x, e) ->
      Printf.sprintf "λ%s. %s" x (string_of_expr_prec current_prec e)
    | App (e1, e2) ->
      let left = string_of_expr_prec current_prec e1 in
      let right = string_of_expr_prec (current_prec + 1) e2 in
      Printf.sprintf "%s %s" left right
    | Let (x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x
        (string_of_expr_prec current_prec e1)
        (string_of_expr_prec current_prec e2)
    | If (e1, e2, e3) ->
      Printf.sprintf "@if %s @then %s @else %s"
        (string_of_expr_prec (current_prec + 1) e1)
        (string_of_expr_prec (current_prec + 1) e2)
        (string_of_expr_prec current_prec e3)
    | Seq (e1, e2) ->
      Printf.sprintf "%s; %s"
        (string_of_expr_prec current_prec e1)
        (string_of_expr_prec current_prec e2)
    | IsZero e ->
      Printf.sprintf "@iszero %s" (string_of_expr_prec (current_prec + 1) e)
    | Succ e ->
      Printf.sprintf "@succ %s" (string_of_expr_prec (current_prec + 1) e)
    | Pred e ->
      Printf.sprintf "@pred %s" (string_of_expr_prec (current_prec + 1) e)
    | Print e ->
      Printf.sprintf "@print %s" (string_of_expr_prec (current_prec + 1) e)
    | Tuple [ e ] ->
      Printf.sprintf "@tuple %s" (string_of_expr_prec (current_prec + 1) e)
    | Tuple l ->
      let rec construct_app l =
        match l with
        | [] -> failwith "Internal error: Unexpected nullary tuple"
        | [ e ] -> Tuple [ e ]
        | [ last; head ] -> App (Tuple [ head ], last)
        | last :: init -> App (construct_app init, last)
      in
      string_of_expr_prec current_prec (construct_app (List.rev l))
  in
  if needs_parens then "(" ^ result ^ ")" else result

and string_of_value = function
  | Num n -> string_of_int n
  | Bool true -> "@true"
  | Bool false -> "@false"
  | Str s -> "\"" ^ s ^ "\""
  | List l ->
    let rec string_of_list l =
      match l with
      | [] -> ""
      | x :: xs -> string_of_value x ^ ", " ^ string_of_list xs
    in
    string_of_list l
  | Unit -> "()"

let string_of_expr = string_of_expr_prec 0
