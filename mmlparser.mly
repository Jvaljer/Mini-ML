(* Mini-ML Parser *)
%{
  (*open Lexing*)
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
(*lists*)
%token LBRACKET RBRACKET
%token ARRAY
%token COMMA
(*End Of File*)
%token EOF


(* listing all the priorities (might need to check the order) *)

(* Starting with all the non associative tokens *)
(*
%nonassoc IF
%nonassoc IN
%nonassoc THEN 
%nonassoc ELSE
%nonassoc NOT
%nonassoc LBRACE RBRACE
%nonassoc LARROW 
%nonassoc BOOL PARS IDENT CST 
%nonassoc LBRACKET RBRACKET (*yet cannot do list of list so cannot be associative*)
(* Then all the associative rules*)
%left PLUS MINUS 
%left MUL DIV 
%left NEG 
%left EQ NEQ 
%left LT LE
%left MOD 
%left OR 
%left AND 
%left SEMI
%right RARROW (* is associative (not as LARROW) *)
%left REV 
*)

%nonassoc IN
%right RARROW 
%nonassoc THEN
%nonassoc ELSE 
%left EQ NEQ
%left LT LE
%left PLUS MINUS
%left MUL DIV 
%left MOD
%left OR
%left AND
%nonassoc LPAR LBRACE 
%nonassoc PARS IDENT CST BOOL 
%left SEMI
%nonassoc LARROW




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
    { if mut!=None then (id,t,true) else (id,t,false) }
;

typ:
  | TINT     { TInt } (*Integer*)
  | TBOOL    { TBool } (*Boolean*)
  | TUNIT    { TUnit } (*Unit*)
  | id=IDENT { TStrct(id) } (*Ident*)
  | t1=typ RARROW t2=typ 
             { TFun(t1, t2) } (*type -> type*)
  | LPAR t=typ RPAR 
             { t } (* ( type ) *)
;


s_expr:
  | n=CST    { Int(n) } (* n *)
  | b=BOOL   { Bool(b) } (* true & false *)
  | PARS     { Unit } (* () *)
  | x=IDENT  { Var(x) } (* ident *)
  | se=s_expr DOT f=IDENT 
             { GetF(se, f) } (* s_expr.ident *)
  | LBRACE id_list=nonempty_list(id_def) RBRACE 
             { Strct(id_list) } (* { [ident = expr ;]+ } *)
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
  | op=unop se=s_expr         { Uop(op, se) } (* uop expr *)
  | e=expr se=s_expr          { App(e, se) } (* expr s_expr *)
  | IF c=expr THEN e=expr     { If(c,e,Unit) } (* if expr then expr *)
  | IF c=expr THEN e1=expr ELSE e2=expr 
                              { If(c,e1,e2) } (* if expr then expr else expr *)
  (*might need to correct this*)
  | FUN f=fun_arg RARROW e=expr { Fun(fst(f),snd(f),e) } (* fun ident -> expr *)
  | LET id=IDENT args=list(fun_arg) ASS e1=expr IN e2=expr 
                              { let f = mk_fun args e1 in
                                Let(id, f , e2) } (* let ident [(ident : type)]* = expr in expr *)
  | LET REC id=IDENT args=list(fun_arg) COLON t=typ ASS e1=expr IN e2=expr 
                              { let f = mk_fun args e1 in
                                let fix = Fix(id, t, f) in
                                Let(id, fix, e2) } (* let rec ident [(ident : type)]* : type = expr in expr *)
  | se=s_expr DOT f=IDENT LARROW e=expr 
                              { SetF(se, f, e) } (* s_expr.ident <- expr *)
  | e1=expr SEMI e2=expr      { Seq(e1, e2) } (* expr ; expr *)
  | LET ARRAY id=IDENT ASS LBRACKET elems=list(list_elem) RBRACKET SEMI 
                              { ArrayInt(id,elems) }
;

list_elem:
  | n=s_expr COMMA { n }
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
  | MINUS { Sub } (* - *)
  | DIV   { Div } (* / *)
  | MOD   { Mod } (* mod *)
  | EQ    { Eq } (* == *)
  | NEQ   { Neq } (* != *)
  | LT    { Lt } (* < *)
  | LE    { Le } (* <= *)
  | AND   { And } (* && *)
  | OR    { Or } (* || *)
;

