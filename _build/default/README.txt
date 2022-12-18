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
Structures |    X    |    X     |       *       |      ~        |
Fix Point  |    X    |    X     |               |               |

##### in-sighted Extensions #####

Global Analysis improvements such as :
  Analysis Refinements -> 
    returning specifics error messages
    when not finding an ident, proposing (in an error message) a possible matching ident (name similitaries)

General Ameliorations such as :
  Printer improvement to make the file as readable & natural as possible

Adding contents to the langage :
  arrays (homogens)
  enumerated types 
  () function argument (empty)
