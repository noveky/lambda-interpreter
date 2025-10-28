%{
open Ast
%}

%token <string> IDENT
%token <int> NUMBER
%token <string> STRING
%token LAMBDA DOT LPAREN RPAREN DSEMICOLON SEMICOLON COMMA LET EQUAL IN TRUE FALSE IF THEN ELSE ISZERO SUCC PRED PRINT PRINTLN PRINTBYTE INCLUDE EVAL STEP EXIT
%token EOF

%right IN
%right ELSE
%right SEMICOLON
%right DOT

%start main
%type <Ast.stmt list> main

%%

main:
  | stmt_list EOF  { $1 }

stmt_list:
  | /* empty */  { [] }
  | stmt DSEMICOLON stmt_list  { $1 :: $3 }
  | stmt stmt_list  { $1 :: $2 }

stmt:
  | LET IDENT EQUAL expr  { Assign ($2, $4) }
  | INCLUDE STRING  { Include $2 }
  | EVAL expr  { Eval $2 }
  | STEP expr  { Step $2 }
  | EXIT NUMBER  { Exit $2 }

expr:
  | LET IDENT EQUAL expr IN expr %prec IN  { Let ($2, $4, $6) }
  | LAMBDA IDENT DOT expr  { Abs($2, $4) }
  | IF expr THEN expr ELSE expr  { If ($2, $4, $6) }
  | expr SEMICOLON expr  { Seq ($1, $3) }
  | app_expr  { $1 }

app_expr:
  | ISZERO atom  { IsZero $2 }
  | SUCC atom  { Succ $2 }
  | PRED atom  { Pred $2 }
  | PRINT atom  { Print $2 }
  | PRINTLN atom  { PrintLn $2 }
  | PRINTBYTE atom  { PrintByte $2 }
  | app_expr atom  { App ($1, $2) }
  | atom  { $1 }

atom:
  | IDENT  { Var $1 }
  | NUMBER  { Val (Num $1) }
  | TRUE  { Val (Bool true) }
  | FALSE  { Val (Bool false) }
  | STRING  { Val (Str $1) }
  | LPAREN tuple_exprs RPAREN  {
      match $2 with
      | (false, []) -> Val Unit
      | (false, [e]) -> e
      | (_, es) -> Tuple es
    }

tuple_exprs:
  | /* empty */  { (false, []) }
  | expr  { (false, [$1]) }
  | expr COMMA tuple_exprs  { let (_, es) = $3 in (true, $1 :: es) }
