(* TypeChecking program for Mini-ML *)
open Mml

(* Typing Environment : matching types with used variables *)
module SymTbl = Map.Make(String)
type tenv = typ SymTbl.t

(* Errors we are allowing (and perhaps wanting) to occure *)
exception Type_error of string
let error s = raise (Type_error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s but got %s" 
           (typ_to_string ty_expected) (typ_to_string ty_actual))
(* may be completed later *)

(* Mini-ML program type checking *)
let type_prog prog =

  (* checks if the Expr [e] is well typed *)
  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  (* Calculates the Exre [e]'s type *)
  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bop((Add | Mul), e1, e2) -> 
       check e1 TInt tenv; check e2 TInt tenv; TInt

  in

  type_expr prog.code SymTbl.empty
