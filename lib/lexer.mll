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
  | "TRUE"                  { TRUE }
  | "FALSE"                 { FALSE }
  | "IF"                    { IF }
  | "THEN"                  { THEN }
  | "ELSE"                  { ELSE }
  | "ISZERO"                { ISZERO }
  | "SUCC"                  { SUCC }
  | "PRED"                  { PRED }
  | "PRINT"                 { PRINT }
  | "PRINTLN"               { PRINTLN }
  | "PRINTBYTE"             { PRINTBYTE }
  | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_''\'']*
                            { IDENT (Lexing.lexeme lexbuf) }
  | '0' | ['1'-'9']['0'-'9']* as n
                            { NUMBER (int_of_string n) }
  | "\"" ([^ '"']* as s) "\""
                            { STRING s }
  | "@include"              { INCLUDE }
  | "@eval"                 { EVAL }
  | "@step"                 { STEP }
  | "@exit"                 { EXIT }
  | eof                     { EOF }
  | _ as c                  {
      let pos = lexbuf.Lexing.lex_curr_p in
      let filename = pos.pos_fname in
      failwith (Printf.sprintf "Lexical error at %s:%d:%d: unexpected character ‘%c’"
        filename pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) c)
    }
