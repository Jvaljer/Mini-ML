(* TypeChecking program for Mini-ML *)
open Mml

(* Typing Environment : matching types with used variables *)
module TypEnv = Map.Make(String)
type tenv = typ TypEnv.t

(* Errors we are allowing (and perhaps wanting) to occure *)
exception Type_error of string
let error s = raise (Type_error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s but got %s" 
           (Mmlpp.typ_to_string ty_expected) (Mmlpp.typ_to_string ty_actual))
(* may be completed later *)

(* In orde rto handle the structures, some external functions will be neccessary...*)
let rec goThrough strct strct' = 
  match (strct,strct') with 
    | [],[] -> Some id
    | (id,e)::l, (id',e')::l' -> if id=id' then
                                      if type_expr e tenv <> type_expr e' tenv then None
                                      else iter l l'
                                    else None 

let rec typeCheckStruct strctList strct = 
  (* we wanna find a struct in the overall structList *)
  match strctList with
    | [] -> assert false (* nothign to do *)
    (* we found the struct 'id' -> *)
    | (id,expr)::st -> match goThrough(s,s') with 
                        | Some id -> TStruct(id) 
                        | None -> typeCheckStruct strct

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
    | Var x -> TypEnv.find x tenv 
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
    | Fun(f, t, e) -> (* we want to recognize the f-called function as t-typed -> add it to the Env*)
                      let t' = type_expr e (TypEnv.add f t tenv) in
                      TFun(t,t');
    | Let(id, e1, e2) -> (* same idea but must 'create' the type first*)
                         let t = type_expr e1 tenv in
                         type_expr e2 (TypEnv.add id t tenv) 
    | App(f1, f2) -> (* App() is the application of a function inside a function *)
                     ( match type_expr f1 tenv with 
                        | TFun(t,t') -> check f2 t tenv; t' (* we want f2 to be well-typed as an f1 argument*)
                        | _ -> assert false ) 
    (* Structures *)
    | Strct s -> typeCheckStruct prog.types s (* might need to correct these *)
    | GetF(e,f) -> assert false 
    | SetF(e1, f, e2) -> assert false 
    (* Fix Point *)
    | Fix(f, t, e) -> assert false 
    (* Other cases *)
    | Seq(e1, e2) -> let _ = type_expr e1 tenv in
                     type_expr e2 tenv;

  in

  type_expr prog.code TypEnv.empty
