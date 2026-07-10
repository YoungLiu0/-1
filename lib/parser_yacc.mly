%{
open Ast
%}

%token <int> NUMBER
%token <string> ID
%token CONST INT VOID
%token IF ELSE WHILE BREAK CONTINUE RETURN
%token OR AND
%token LE GE EQ NE LT GT
%token PLUS MINUS STAR SLASH PERCENT
%token NOT ASSIGN
%token SEMI COMMA LPAREN RPAREN LBRACE RBRACE
%token EOF

%left OR
%left AND
%left EQ NE
%left LT LE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc UMINUS
%nonassoc THEN
%nonassoc ELSE

%start <Ast.program> program
%%

program:
  | top_defs EOF { $1 }

top_defs:
  | top_def          { [$1] }
  | top_def top_defs { $1 :: $2 }

top_def:
  | func_def { FuncDef $1 }
  | INT ID ASSIGN expr SEMI          { GlobalVarDecl ($2, $4) }
  | CONST INT ID ASSIGN expr SEMI    { GlobalConstDecl ($3, $5) }

func_def:
  | INT ID LPAREN params RPAREN block
      { { f_name = $2; f_type = Int; f_params = $4; f_body = $6 } }
  | VOID ID LPAREN params RPAREN block
      { { f_name = $2; f_type = Void; f_params = $4; f_body = $6 } }

params:
  | /* empty */ { [] }
  | param_list  { $1 }

param_list:
  | INT ID                { [$2] }
  | INT ID COMMA param_list { $2 :: $4 }

block:
  | LBRACE stmt* RBRACE { Block $2 }

stmt:
  | block { $1 }
  | SEMI { EmptyStmt }
  | expr SEMI { ExprStmt $1 }
  | ID ASSIGN expr SEMI { Assign ($1, $3) }
  | CONST INT ID ASSIGN expr SEMI { ConstDecl ($3, $5) }
  | INT ID ASSIGN expr SEMI { VarDecl ($2, $4) }
  | IF LPAREN expr RPAREN stmt %prec THEN { If ($3, $5, None) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If ($3, $5, Some $7) }
  | WHILE LPAREN expr RPAREN stmt { While ($3, $5) }
  | BREAK SEMI { Break }
  | CONTINUE SEMI { Continue }
  | RETURN expr SEMI { Return (Some $2) }
  | RETURN SEMI { Return None }

expr:
  | lor_expr { $1 }

lor_expr:
  | land_expr { $1 }
  | lor_expr OR land_expr { Binary (Or, $1, $3) }

land_expr:
  | rel_expr { $1 }
  | land_expr AND rel_expr { Binary (And, $1, $3) }

rel_expr:
  | add_expr { $1 }
  | rel_expr LT add_expr { Binary (Lt, $1, $3) }
  | rel_expr GT add_expr { Binary (Gt, $1, $3) }
  | rel_expr LE add_expr { Binary (Le, $1, $3) }
  | rel_expr GE add_expr { Binary (Ge, $1, $3) }
  | rel_expr EQ add_expr { Binary (Eq, $1, $3) }
  | rel_expr NE add_expr { Binary (Ne, $1, $3) }

add_expr:
  | mul_expr { $1 }
  | add_expr PLUS mul_expr { Binary (Add, $1, $3) }
  | add_expr MINUS mul_expr { Binary (Sub, $1, $3) }

mul_expr:
  | unary_expr { $1 }
  | mul_expr STAR unary_expr { Binary (Mul, $1, $3) }
  | mul_expr SLASH unary_expr { Binary (Div, $1, $3) }
  | mul_expr PERCENT unary_expr { Binary (Mod, $1, $3) }

unary_expr:
  | primary_expr { $1 }
  | PLUS unary_expr { Unary (Pos, $2) }
  | MINUS unary_expr %prec UMINUS { Unary (Neg, $2) }
  | NOT unary_expr { Unary (Not, $2) }

primary_expr:
  | ID { Var $1 }
  | NUMBER { IntLit $1 }
  | LPAREN expr RPAREN { $2 }
  | ID LPAREN args RPAREN { Call ($1, $3) }

args:
  | /* empty */ { [] }
  | expr_list { $1 }

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }