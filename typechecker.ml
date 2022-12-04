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
           (Mmlpp.typ_to_string ty_expected) (Mmlpp.typ_to_string ty_actual))
(* may be completed later *)

(* Mini-ML program type checking *)
let type_prog prog =

  (* checks if the Expr [e] is well typed *)
  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  (* Calculates the Exre [e]'s type *)
  and type_expr e tenv = match e with
    (* simple cases (enonciated types & Variables)*)
    | Int _  -> TInt
    | Bool _ -> TBool
    | Unit -> TUnit 
    | Var x -> SymTbl.find x tenv 
    (* Arithmetic Operands *)
      (* Unary Operands *)
    | Uop(Neg, e) -> check e TInt tenv; TInt
    | Uop(Not, e) -> check e TBool tenv; TBool
      (* Binary Operands *)
    | Bop((Add | Mul | Sub | Div | Mod), e1, e2) -> check e1 TInt tenv; check e2 TInt tenv; TInt
    | Bop((And | Or), e1, e2) -> check e1 TBool tenv; check e2 TBool tenv; TBool
    | Bop((Lt | Le), e1, e2) -> check e1 TInt tenv; check e2 TInt tenv; TBool
    | Bop((Eq | Neq), e1, e2) -> check e1 (type_expr e2 tenv) tenv; TBool
    (* Conditions *)
    | If(c, e1, e2) -> check c TBool tenv; 
                       let t1 = type_expr e1 tenv in
                       check e2 t1 tenv;
                       t1 (* and what if we want 2 different typed expr *)
    (* Functions *)
    | Fun(f, t, e) -> assert false 
    | Let(id, e1, e2) -> assert false 
    | App(e1, e2) -> assert false 
    (* Structures *)
    | Strct s -> assert false 
    | GetF(e,f) -> assert false 
    | SetF(e1, f, e2) -> assert false 
    (* Fix Point *)
    | Fix(f, t, e) -> assert false 
    (* Other cases *)
    | Seq(e1, e2) -> let t1 = type_expr e1 tenv in
                     type_expr e2 tenv;

  in

  type_expr prog.code SymTbl.empty
