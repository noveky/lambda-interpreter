type stmt =
  | Assign of string * expr
  | Include of string
  | Eval of expr
  | Step of expr
  | Exit of int

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
  | PrintLn of expr
  | PrintByte of expr
  | Tuple of expr list
  | Wrong of string

and value =
  | Num of int
  | Bool of bool
  | Str of string
  | List of value list
  | Unit

let precedence = function
  | Var _ | Val _ | Tuple _ -> 6
  | App _ | IsZero _ | Succ _ | Pred _ | Print _ | PrintLn _ | PrintByte _
  | Wrong _ ->
    5
  | Abs _ -> 4
  | Seq _ -> 3
  | If _ -> 2
  | Let _ -> 1

let is_right_associative = function Abs _ -> true | _ -> false

let rec string_of_expr_prec parent_prec print_mode expr =
  let current_prec = precedence expr in
  let needs_parens = current_prec < parent_prec in
  let result =
    match expr with
    | Var x -> x
    | Val v -> string_of_value print_mode v
    | Wrong s -> Printf.sprintf "WRONG \"%s\"" s
    | Abs (x, e) ->
      Printf.sprintf "λ%s. %s" x (string_of_expr_prec current_prec print_mode e)
    | App (e1, e2) ->
      let left = string_of_expr_prec current_prec print_mode e1 in
      let right = string_of_expr_prec (current_prec + 1) print_mode e2 in
      Printf.sprintf "%s %s" left right
    | Let (x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x
        (string_of_expr_prec current_prec print_mode e1)
        (string_of_expr_prec current_prec print_mode e2)
    | If (e1, e2, e3) ->
      Printf.sprintf "IF %s THEN %s ELSE %s"
        (string_of_expr_prec (current_prec + 1) print_mode e1)
        (string_of_expr_prec (current_prec + 1) print_mode e2)
        (string_of_expr_prec current_prec print_mode e3)
    | Seq (e1, e2) ->
      Printf.sprintf "%s; %s"
        (string_of_expr_prec current_prec print_mode e1)
        (string_of_expr_prec current_prec print_mode e2)
    | IsZero e ->
      Printf.sprintf "ISZERO %s"
        (string_of_expr_prec (current_prec + 1) print_mode e)
    | Succ e ->
      Printf.sprintf "SUCC %s"
        (string_of_expr_prec (current_prec + 1) print_mode e)
    | Pred e ->
      Printf.sprintf "PRED %s"
        (string_of_expr_prec (current_prec + 1) print_mode e)
    | Print e | PrintLn e | PrintByte e ->
      Printf.sprintf "%s %s"
        (match expr with
        | Print _ -> "PRINT"
        | PrintLn _ -> "PRINTLN"
        | PrintByte _ -> "PRINTBYTE"
        | _ -> assert false)
        (string_of_expr_prec (current_prec + 1) print_mode e)
    | Tuple l ->
      let elements = List.map (string_of_expr_prec 0 print_mode) l in
      let joined = String.concat ", " elements in
      Printf.sprintf "(%s%s)" joined (if List.length l = 1 then "," else "")
  in
  if needs_parens then "(" ^ result ^ ")" else result

and string_of_value print_mode = function
  | Num n -> string_of_int n
  | Bool true -> "TRUE"
  | Bool false -> "FALSE"
  | Str s -> if print_mode then s else "\"" ^ s ^ "\""
  | List l ->
    let rec string_of_list l =
      match l with
      | [] -> ""
      | x :: xs -> string_of_value print_mode x ^ ", " ^ string_of_list xs
    in
    string_of_list l
  | Unit -> if print_mode then "" else "()"

let string_of_expr = string_of_expr_prec 0
