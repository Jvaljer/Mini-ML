(* Mini-ML Parser *)
%{
  open Lexing
  open Mml

%}

(*all possible tokens recognized by the Lexer*)
(*constants & variables*)
%token <int> CST 
%token <bool> BOOL 
%token <string> IDENT
%token PARS 
(*Binary Operand*)
  (*numeral*)
%token PLUS MUL MINUS DIV MOD
  (*boolean*)
%token AND OR EQ NEQ LT LE 
(*Unary Operand*)
%token NEG NOT
(*functions*)
%token FUN LET REC IN 
(*types*)
%token TYPE TUNIT TBOOL TINT MUTABLE
(*conditions*)
%token IF THEN ELSE
(*syntax characters*)
%token DOT COLON SEMI LARROW RARROW ASS 
(*parenthesis*)
%token LPAR RPAR LBRACE RBRACE
(*End Of File*)
%token EOF


(*priorities must be defined*)

%start program
%type <Mml.prog> program

%%

(*pattern recognization as described in the MML Grammar*)

program:
  (* program := [type_def ]* expr eof *)
  | types=list(type_def) code=expr EOF 
    { {types ; code} }
;

type_def:
  (* type_def := type ident = { [[mutable]? ident : type ;]+ } *)
  | TYPE t_name=IDENT ASS LBRACE t_def=nonempty_list(type_def_arg) RBRACE
    { (t_name,t_def) } 
;

type_def_arg:
  (* [mutable]? ident : type ;*)
  | mut=option(MUTABLE) id=IDENT COLON t=typ SEMI
    { if mut then (id,t,true) else (id,t,false) }
;

typ:
  | TINT     { TInt } (*Integer*)
  | TBOOL    { TBool } (*Boolean*)
  | TUNIT    { TUnit } (*Unit*)
  | id=IDENT { IDENT(id) } (*Ident*)
  | t1=type RARROW t2=typ 
             { (*complete*)} (*type -> type*)
  | LPAR t=typ RPAR 
             { t } (* ( type ) *)
;


s_expr:
  | n=CST    { Int(n) } (* n *)
  | b=BOOL   { Bool(b) } (* true & false *)
  | PARS     { Unit } (* () *)
  | x=IDENT { Var(x) } (* ident *)
  | se=s_expr DOT id=IDENT 
             { (*complete*) } (* s_expr.ident *)
  | LBRACE id_list=nonempty_list(id_def) RBRACE 
             { (*complete*)} (* { [ident = expr ;]+ } *)
  | LPAR e=expr RPAR 
             { e } (* ( expr ) *)
;

id_def:
  (* ident = expr ;*)
  | id=IDENT ASS e=expr SEMI 
    { (id,e) }
;

expr:
  | se=s_expr                 { se } (* s_expr *)
  | e1=expr op=binop e2=expr  { Bop(op, e1, e2) } (* expr bop expr *)
  | op=unop se=s_expr         { Uop(op,se) } (* uop expr *)
  | e=expr se=s_expr          { (*complete*) } (* expr s_expr *)
  | IF c=expr THEN e=expr     { If(c,e1,Unit) } (* if expr then expr *)
  | IF c=expr THEN e1=expr ELSE e2=expr 
                              { If(c,e1,e2) } (* if expr then expr else expr *)
  (*might need to correct this*)
  | FUN f=IDENT RARROW e=expr { Fun(f,_,e) } (* fun ident -> expr *)
  (*must correct these 2*)
  | LET id=IDENT args=list(fun_arg) ASS e1=expr IN e2=expr 
                              { Let(id,e1,e2) } (* let ident [(ident : type)]* = expr in expr *)
  | LET REC id=IDENT args=list(fun_arg) COLON e1=expr IN e2=expr 
                              { Let(id,e1,e2) } (* let rec ident [(ident : type)]* : type = expr in expr *)
  | se=s_expr DOT x=IDENT LARROW e=expr 
                              { (*complete*) } (* s_expr.ident <- expr *)
  | e1=expr SEMI e2=expr      { (*complete*) } (* expr ; expr *)
;

fun_arg:
  (* ( ident : type ) *)
  | LPAR id=IDENT COLON t=typ RPAR 
    { (id,t) }
;
%inline unop:
  | NOT { Not } (* not *)
  | NEG { Neg } (* - *)
;

%inline binop:
  | PLUS  { Add } (* + *)
  | MUL   { Mul } (* * *)
  | MINUS { Minus } (* - *)
  | DIV   { DIV } (* / *)
  | MOD   { Mod } (* mod *)
  | EQ    { Eq } (* == *)
  | NEQ   { Neq } (* != *)
  | LT    { Lt } (* < *)
  | LE    { Le } (* <= *)
  | AND   { And } (* && *)
  | OR    { Or } (* || *)
;
