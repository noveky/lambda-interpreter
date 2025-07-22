open Liblambda

let usage = "Usage: lambda [options] [file]"

let parse_args () =
  let files = ref [] in
  let options = [] in
  let anon_fun filename = files := !files @ [ filename ] in
  Arg.parse options anon_fun usage;
  !files

let () =
  try
    let files = parse_args () in
    if files = [] then ignore (Interpreter.exec_file [] stdin)
    else
      let rec exec_files env = function
        | [] -> env
        | file :: rest ->
          let ic = open_in file in
          let env = Interpreter.exec_file env ic in
          exec_files env rest
      in
      ignore (exec_files [] files)
  with
  | Sys_error msg ->
    Printf.eprintf "%s\n" msg;
    exit 1
  | Parsing.Parse_error ->
    Printf.eprintf "Parse error\n";
    exit 1
  | Failure msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
  | Arg.Bad msg ->
    Printf.eprintf "%s\n%s\n" msg usage;
    exit 1
