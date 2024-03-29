# MINI-ML # 
university project which goal is to encode by ourself a Caml-like langage called 'Mini-ML' .

Link to Thibault Balabonski's enunciate for this project :
https://www.lri.fr/~blsk/CompilationLDD3/dm-mml.html

##### Description of MiniML #####

 A mini-ml program is composed of type definitions serie followed by a single expression to interpret .
 
 example (test/syracuse.mml) :
 
```
  type intref = { mutable value: int; }
  type sequence = { start: int; next: int -> int; stop: int -> bool; }

  (* maximal value of a numeric sequence *)
  let max_value (s:sequence) =
    let max = { value = s.start; } in
    let rec iter (n:int): unit =
      if max.value < n then max.value <- n;
      if not (s.stop n) then iter (s.next n)
    in
    iter s.start;
    max.value
  in

  (* collatz conjecture: for any n, this sequence eventually reaches 1 *)
  let syracuse n = {
      start = n;
      next = (fun (k:int) -> if k mod 2 == 0 then k/2 else 3*k+1);
      stop = (fun (k:int) -> k <= 1);
    }
  in

  max_value (syracuse 27)
```


##### MiniML Syntax  #####

prog := [ type_def ]* expr eof

type_def := type ident = { [[mutable]? ident : type ;]+ }

type := int
      | bool 
      | unit
      | ident
      | type -> type 
      | ( type )

expr := s_expr
      | uop s_expr
      | expr bop expr 
      | expr s_expr
      | if expr then expr 
      | if expr then expr else expr 
      | fun ident -> expr 
      | let ident [( ident : type )]* = expr in expr
      | let rec ident [( ident : type )]* : type = expr in expr
      | s_expr . ident <- expr 
      | expr ; expr 

s_expr := n
        | true
        | false
        | ()
        | ident
        | s_expr . ident 
        | { [ident = expr ;]+ }
        | ( expr )

uop := - 
     | not 

bop := + 
     | -
     | *
     | /
     | mod
     | == 
     | != 
     | < 
     | <= 
     | &&
     | ||
*

##### Type Checking rules #####

Constants typing rules :
___________   _______________  _________________
E |- n: int   E |- true: bool   E |- false: bool 

Unary Operand : 
E |- e: int    E |- e: bool
-----------    ------------
E |- e: int   E|- not e: bool

Binary Operand : 
    E |- e1: int   E |- e2: int        E |- e1: int   E |- e2: int
(1) ---------------------------    (2) ---------------------------
        E |- e1 op1 e2 : int                 E |- e1 op2 e2 : bool

    E |- e1: t     E |- e2: t
(3) -------------------------    
      E |- e1 op3 e2 : bool

with op1 := + | - | * | / | mod
     op2 := < | <= | && | || 
     op3 := == | !=

Variables : 
   E(x) = t       E |- e1 : t1     E, x:t1 |- e2 : t2
  ----------      -----------------------------------
  E |- x : t          E |- let x = e1 in e2 : t2 

Conditionnal expression : 
  E |- e0 : bool     E |- e1 : t     E |- e2 : t
  ----------------------------------------------
         E |- if e0 then e1 else e2 : t

Functions and all fun-like stuff :
        E, x:t1 |- e : t2             E |- e1 : t2 -> t1     E |- e2 : t2
  -------------------------------      -----------------------------------
  E |- fun (x:t1) -> e : t1 -> t2                E |- e1 e2 : t1

     E, x:t1 |- e : t2
  -----------------------
  E |- Fix(x, t1, e) : t2

Sequencies Operand : 

  E |- e1 : t1     E |- e2 : t2
  -----------------------------
        E |- e1 ; e2 : t2

Structures :
  E |- e : s     s.x : t      E |- e1 : s     s.x : t mutable     E |- e2 : t
  ----------------------      -----------------------------------------------
       E |- e.x : t                       E |- e1.x <- e2 : unit

  s.x1 : t1     E |- e1 : t1     ...     s.xN : tN     E |- eN : tN
  -----------------------------------------------------------------
                E |- { x1 = e1; ...; xN = eN; } : s


##### Progression #####
I consider going through this project with the 2nd working-method given by T.Balabonski :
  Implementing + Testing each line before going to the next one.

X : Done (quite sure)
~ : Not completed
* : Done but not sure / build error 

           |  Lexer  |  Parser  |  TypeChecker  |  Interpreter  |
Arithmetic |    X    |    X     |       X       |      X        |
Variables  |    X    |    X     |       X       |      X        |
Functions  |    X    |    X     |       X       |      X        |
Structures |    X    |    X     |       X       |      X        |
Fix Point  |    X    |    X     |       X       |      X        |


Extensions I added : 
  Integer Arrays (only declaration...)
  Matching Pattern 
  Correct Errors 
  Uniform Arrays (only declaration...)

##### in-sighted Extensions #####

Int Arrays : (first overall idea)
    as an expr ? ==> mml.ml -  ArrayInt of string * (int/expr) list
    recognized as ? ==> mmlparser.mly -  let array l = [int,int,int, ... ,];
                                        LET ARRAY id=IDENT ASS LBRACKET elems=list_elm RBRACKET SEMI { ArrayInt(id,elems) }
                                        list_elm: | n=CST COMMA { Int(n) } (*how to make the last elem without comma ?*)
    typechecked as ? ==> typechecker.ml -  must check for each element in 'elems' if it's well typed TInt 
                                           then return the type TArrayInt (without any arhument !)
    interpreted as ? ==> interpreter.ml -  return a list of VInt ? (might use (VInt) list) 
                                           a VArrayInt could be nice...
    printed as ? ==> rly don't wanna do this... 

    ANOTHER COMPLETION :: maybe could add something to get a special element ? like GetI, how to do ???


Matching pattern :(not any idea of how to do)
    as an expr for sure ==> mml.ml -  MatchPattern of expr * (expr) list * (expr) list * expr ??? 
    recognized as ? ==> mmlparser.mly -  match 'expr' with 
                                         | 'expr_bis' -> 'expr_bis_consequence'
                                         ...
                                         | 'expr_n-1' -> 'expr_n-1_consequence' 
                                         | '_' -> 'expr_n_consequence'
    typechecked as ? ==> typechecker.ml -  must check if 'expr' is of same type as all 'expr_bis' -> 'expr_n' ? 
                                           '_' expr is needed to be associated to 'expr_n' !
                                           then must check if all 'expr_i_consequence' is well typed ...
    intepreted as ? ==> intepreter.ml -  if 'expr' == 'expr_i" then 'expr_i_consequence' else 'expr_n_consequence' ? 

Correct Errors implementation (shall not be very hard but seems important to do...) -> Tried to do it properly in the TypeChecker


Uniform Arrays ???
