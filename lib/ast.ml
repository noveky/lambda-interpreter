type stmt = Assign of string * expr

and expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Val of value
  | If of expr * expr * expr
  | IsZero of expr
  | Succ of expr
  | Pred of expr
  | Wrong

and value = Num of int | Bool of bool

let precedence = function
  | Var _ | Val _ | Wrong -> 6
  | IsZero _ | Succ _ | Pred _ -> 5
  | App _ -> 4
  | If _ -> 3
  | Let _ -> 2
  | Abs _ -> 1

let is_right_associative = function Abs _ -> true | _ -> false

let rec string_of_expr_prec parent_prec expr =
  let current_prec = precedence expr in
  let needs_parens = current_prec < parent_prec in
  let result =
    match expr with
    | Var x -> x
    | Val v -> string_of_value v
    | Wrong -> "wrong"
    | Abs (x, e) -> Printf.sprintf "λ%s. %s" x (string_of_expr_prec 0 e)
    | App (e1, e2) ->
      let left = string_of_expr_prec current_prec e1 in
      let right = string_of_expr_prec (current_prec + 1) e2 in
      Printf.sprintf "%s %s" left right
    | Let (x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x (string_of_expr_prec 0 e1)
        (string_of_expr_prec 0 e2)
    | If (e1, e2, e3) ->
      Printf.sprintf "if %s then %s else %s" (string_of_expr_prec 0 e1)
        (string_of_expr_prec 0 e2)
        (string_of_expr_prec (current_prec + 1) e3)
    | IsZero e ->
      Printf.sprintf "iszero %s" (string_of_expr_prec (current_prec + 1) e)
    | Succ e ->
      Printf.sprintf "succ %s" (string_of_expr_prec (current_prec + 1) e)
    | Pred e ->
      Printf.sprintf "pred %s" (string_of_expr_prec (current_prec + 1) e)
  in
  if needs_parens then "(" ^ result ^ ")" else result

and string_of_value = function
  | Num n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"

let string_of_expr = string_of_expr_prec 0
