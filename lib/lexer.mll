{
open Parser
}

rule main = parse
  | [' ' '\t' '\r' '\n']    { main lexbuf }
  | "#" [^ '\n']* '\n'     { main lexbuf }
  | "λ" | "\\"              { LAMBDA }
  | "."                     { DOT }
  | "("                     { LPAREN }
  | ")"                     { RPAREN }
  | ";"                     { SEMICOLON }
  | "let"                   { LET }
  | "="                     { EQUAL }
  | "in"                    { IN }
  | "true"                  { TRUE }
  | "false"                 { FALSE }
  | "if"                    { IF }
  | "then"                  { THEN }
  | "else"                  { ELSE }
  | "iszero"                { ISZERO }
  | "succ"                  { SUCC }
  | "pred"                  { PRED }
  | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*
                            { IDENT (Lexing.lexeme lexbuf) }
  | '0' | ['1'-'9']['0'-'9']* as n
                            { NUMBER (int_of_string n) }
  | eof                     { EOF }
  | _ as c                  { failwith (Printf.sprintf "Unrecognized char: %c" c) }
