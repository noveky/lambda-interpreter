open Liblambda

let () =
  try ignore (Interpreter.exec_file [] stdin) with
  | Parsing.Parse_error ->
    Printf.eprintf "Parse error\n";
    exit 1
  | Failure msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
