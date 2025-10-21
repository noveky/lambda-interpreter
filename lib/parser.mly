%{
open Ast
%}

%token <string> IDENT
%token <int> NUMBER
%token <string> STRING
%token LAMBDA DOT LPAREN RPAREN DSEMICOLON SEMICOLON LET EQUAL IN TRUE FALSE IF THEN ELSE ISZERO SUCC PRED TUPLE PRINT INCLUDE STEP
%token EOF

%right DSEMICOLON
%nonassoc EQUAL INCLUDE STEP
%right LET IN
%right IF THEN ELSE
%right SEMICOLON
%right LAMBDA DOT
%left ISZERO SUCC PRED TUPLE PRINT
%left APP

%start main
%type <Ast.stmt list> main

%%

main:
  | stmt_list EOF  { $1 }

stmt_list:
  | stmt DSEMICOLON stmt_list  { $1 :: $3 }
  | /* empty */  { [] }

stmt:
  | IDENT EQUAL expr  { Assign ($1, $3) }
  | expr  { Eval $1 }
  | INCLUDE STRING  { Include $2 }
  | STEP expr  { Step $2 }

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
  | TUPLE atom  { Tuple [$2] }
  | app_expr atom %prec APP  { App ($1, $2) }
  | atom  { $1 }

atom:
  | IDENT  { Var $1 }
  | NUMBER  { Val (Num $1) }
  | TRUE  { Val (Bool true) }
  | FALSE  { Val (Bool false) }
  | STRING  { Val (Str $1) }
  | LPAREN expr RPAREN  { $2 }
