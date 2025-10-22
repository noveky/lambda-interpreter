{
open Parser
}

rule main = parse
  | [' ' '\t' '\r']         { main lexbuf }
  | '\n'                    { Lexing.new_line lexbuf; main lexbuf }
  | "#" [^ '\n']* '\n'      { Lexing.new_line lexbuf; main lexbuf }
  | "λ" | "\\"              { LAMBDA }
  | "."                     { DOT }
  | "("                     { LPAREN }
  | ")"                     { RPAREN }
  | ";;"                    { DSEMICOLON }
  | ";"                     { SEMICOLON }
  | ","                     { COMMA }
  | "let"                   { LET }
  | "="                     { EQUAL }
  | "in"                    { IN }
  | "@true"                 { TRUE }
  | "@false"                { FALSE }
  | "@if"                   { IF }
  | "@then"                 { THEN }
  | "@else"                 { ELSE }
  | "@iszero"               { ISZERO }
  | "@succ"                 { SUCC }
  | "@pred"                 { PRED }
  | "@print"                { PRINT }
  | "@println"              { PRINTLN }
  | "@tuple"                { TUPLE }
  | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_''\'']*
                            { IDENT (Lexing.lexeme lexbuf) }
  | '0' | ['1'-'9']['0'-'9']* as n
                            { NUMBER (int_of_string n) }
  | "\"" ([^ '"']* as s) "\""
                            { STRING s }
  | "@include"              { INCLUDE }
  | "@eval"                 { EVAL }
  | "@step"                 { STEP }
  | eof                     { EOF }
  | _ as c                  {
      let pos = lexbuf.Lexing.lex_curr_p in
      failwith (Printf.sprintf "Unexpected character '%c' at line %d, column %d"
        c pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1))
    }
