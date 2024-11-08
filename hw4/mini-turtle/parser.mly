
/* Parsing for mini-Turtle */

%{
  open Ast

%}

/* Declaration of tokens */

// if else def repeat penup pendown forward turnleft
// turnright color black white red green
%token <Ast.binop> BINOP
%token <string> IDENT
%token <int> INT
%token IF ELSE DEF REPEAT PENUP PENDOWN FORWARD 
%token TURNLEFT TURNRIGHT COLOR BLACK WHITE RED GREEN BLUE
%token NEWLINE EOF
%token ADD SUB MUL DIV
%token LP RP LSQ RSQ COMMA COLON

/* Priorities and associativity of tokens */

%left BINOP
%left ADD SUB
%left MUL DIV 

/* Axiom of the grammar */
%start prog

/* Type of values ​​returned by the parser */
%type <Ast.program> prog

%%

/* Production rules of the grammar */

def:
| DEF f = IDENT LP x = separated_list(COMMA, IDENT) RP LSQ NEWLINE s = stmts RSQ
    { { name = f; formals = x; body = Sblock s } }

stmts:
| stmt NEWLINE stmts
    { $1 :: $3 }
| stmt NEWLINE
    { [$1] }
| NEWLINE stmts
    { $2 }
| /* empty */
    { [] }

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
| REPEAT e = expr LSQ s = stmts RSQ
    { Srepeat (e, Sblock s) }
| IF e = expr COLON s1 = stmt ELSE COLON s2 = stmt
    { Sif (e, s1, s2) }
| IF e = expr COLON s = stmt
    { Sif (e, s, Sblock []) }
| REPEAT e = expr COLON s = stmt
    { Srepeat (e, s) }
| id = IDENT LP e = separated_list(COMMA, expr) RP
    { Scall (id, e) }

expr:
| INT
    { Econst $1 }
| IDENT
    { Evar $1 }
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
| NEWLINE? dl = list(def) b = stmts NEWLINE? EOF
    { { defs = dl; main = Sblock b } (* To be modified *) }
;


