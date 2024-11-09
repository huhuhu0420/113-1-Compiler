
/* Parsing for mini-Turtle */

%{
  open Ast

%}

/* Declaration of tokens */

// if else def repeat penup pendown forward turnleft
// turnright color black white red green
%token <string> IDENT
%token <int> INT
%token IF ELSE DEF REPEAT PENUP PENDOWN FORWARD 
%token TURNLEFT TURNRIGHT COLOR BLACK WHITE RED GREEN BLUE
%token EOF
%token ADD SUB MUL DIV
%token LP RP LCURL RCURL COMMA COLON

/* Priorities and associativity of tokens */

%left ADD SUB
%left MUL DIV 
%nonassoc unary_minus

/* Axiom of the grammar */
%start prog

/* Type of values ​​returned by the parser */
%type <Ast.program> prog
%type <Ast.def list> defs
%type <Ast.def> def
%type <Ast.stmt list> stmts
%type <Ast.stmt> stmt
%type <Ast.expr> expr
%type <Turtle.color> color
%type <string list> params
%type <Ast.expr list> args

%%

/* Production rules of the grammar */

params:
| /* empty */
    { [] }
| IDENT
    { [$1] }
| IDENT COMMA params
    { $1 :: $3 }

args:
| /* empty */
    { [] }
| expr
    { [$1] }
| expr COMMA args
    { $1 :: $3 }

defs:
| /* empty */
    { [] } 
| def defs
    { $1 :: $2 }

def:
| DEF f = IDENT LP x = params RP LCURL s = stmts RCURL
    { { name = f; formals = x; body = Sblock s } }
| DEF f = IDENT LP x = params RP s = stmt
    { { name = f; formals = x; body = s } }

stmts:
| /* empty */
    { [] }
| stmt stmts 
    { $1 :: $2 }

stmt:
| PENUP  
    { Spenup }
| PENDOWN
    { Spendown }
| FORWARD e = expr
    { Sforward e }
| TURNLEFT e = expr
    { Sturnleft e }
| TURNRIGHT e = expr
    { Sturnright e }
| COLOR c = color
    { Scolor c }
| REPEAT e = expr LCURL s = stmts RCURL
    { Srepeat (e, Sblock s) }
| IF e = expr LCURL s1 = stmts RCURL ELSE LCURL s2 = stmts RCURL
    { Sif (e, Sblock s1, Sblock s2) }
| IF e = expr LCURL s1 = stmts RCURL ELSE s2 = stmt 
    { Sif (e, Sblock s1, s2) }
| IF e = expr LCURL s = stmts RCURL
    { Sif (e, Sblock s, Sblock []) }
| REPEAT e = expr COLON s = stmt
    { Srepeat (e, s) }
| id = IDENT LP e = args RP
    { Scall (id, e) }

expr:
| INT
    { Econst $1 }
| IDENT
    { Evar $1 }
| SUB e = expr %prec unary_minus
    { Eunop (Neg, e) }
| e1 = expr ADD e2 = expr
    { Ebinop (Add, e1, e2) }
| e1 = expr SUB e2 = expr
    { Ebinop (Sub, e1, e2) }
| e1 = expr MUL e2 = expr
    { Ebinop (Mul, e1, e2) }
| e1 = expr DIV e2 = expr
    { Ebinop (Div, e1, e2) }
| LP e = expr RP
    { e }

color:
| BLACK
    { Turtle.black }
| WHITE
    { Turtle.white }
| RED
    { Turtle.red }
| GREEN
    { Turtle.green }
| BLUE
    { Turtle.blue }

prog:
| dl = defs b = stmts EOF
    { { defs = dl; main = Sblock b } }
;


