%{
open Ast
%}

%token <string> IDENT
%token <int> NUMBER
%token <string> STRING
%token LAMBDA DOT LPAREN RPAREN SEMICOLON LET EQUAL IN TRUE FALSE IF THEN ELSE ISZERO SUCC PRED INCLUDE STEP
%token EOF

%right IF THEN ELSE
%nonassoc ISZERO SUCC PRED

%start main
%type <Ast.stmt list> main

%%

main:
  | stmt_list EOF  { $1 }

stmt_list:
  | stmt_list stmt SEMICOLON  { $1 @ [$2] }
  | /* empty */  { [] }

stmt:
  | IDENT EQUAL expr  { Assign ($1, $3) }
  | expr  { Eval $1 }
  | INCLUDE STRING  { Include $2 }
  | STEP expr  { Step $2 }

expr:
  | LET IDENT EQUAL expr IN expr  { Let ($2, $4, $6) }
  | LAMBDA IDENT DOT expr  { Abs($2, $4) }
  | IF expr THEN expr ELSE expr  { If ($2, $4, $6) }
  | app_expr  { $1 }

app_expr:
  | app_expr atom  { App($1, $2) }
  | ISZERO atom  { IsZero $2 }
  | SUCC atom  { Succ $2 }
  | PRED atom  { Pred $2 }
  | atom  { $1 }

atom:
  | IDENT  { Var $1 }
  | NUMBER  { Val (Num $1) }
  | TRUE  { Val (Bool true) }
  | FALSE  { Val (Bool false) }
  | LPAREN expr RPAREN  { $2 }
