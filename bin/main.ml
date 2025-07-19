open Liblambda

let () =
  let lexbuf =
    let input = Preprocessor.preprocess_lambda stdin in
    Lexing.from_string input
  in
  let stmts, expr =
    try Parser.main Lexer.main lexbuf with
    | Parsing.Parse_error ->
      Printf.eprintf "Parse error\n";
      exit 1
    | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  in
  let env = Interpreter.exec_stmts [] stmts in
  let rec step_loop expr =
    print_endline (Ast.string_of_expr expr);
    let expr' = Interpreter.step env expr in
    if expr' = expr then expr' else step_loop expr'
  in
  ignore (step_loop expr)
(* print_endline (Ast.string_of_expr (Interpreter.eval env expr)) *)
