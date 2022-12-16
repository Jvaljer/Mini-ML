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
    | Bop((Add | Mul | Sub | Div | Mod), e, e') -> check e TInt tenv; check e' TInt tenv; TInt
    | Bop((And | Or), e, e') -> check e TBool tenv; check e' TBool tenv; TBool
    | Bop((Lt | Le), e, e') -> check e TInt tenv; check e' TInt tenv; TBool
    | Bop((Eq | Neq), e, e') -> check e (type_expr e' tenv) tenv; TBool
    (* Conditions *)
    | If(c, e, e') -> check c TBool tenv; 
                       let t = type_expr e tenv in
                       check e' t tenv;
                       t (* and what if we want 2 different typed expr *)
    (* Functions *)
    | Fun(f, t, e) -> (* we want to recognize the f-called function as t-typed -> add it to the Env*)
                      let t' = type_expr e (TypEnv.add f t tenv) in
                      TFun(t,t');
    | Let(id, e, e') -> (* same idea but must 'create' the type first*)
                         let t = type_expr e tenv in
                         type_expr e' (TypEnv.add id t tenv) 
    | App(f, f') -> (* App() is the application of a function inside a function *)
                     ( match type_expr f tenv with 
                        | TFun(t,t') -> check f' t tenv; t' (* we want f2 to be well-typed as an f1 argument*)
                        | _ -> assert false ) 
    (* Structures *)
    | Strct s -> let rec buildStruct = function
                   | [] -> assert true 
                   | (s_name,s_values)::l -> ( let rec iter = function
                                                 | [],[] -> Some s_name
                                                 | (id,e)::l1, (id',t',_)::l2 -> if id=id' then 
                                                                                    if t'<> type_expr e tenv then None 
                                                                                    else iter (l1,l2) 
                                                                                  else None 
                                                 | _, _ -> None
                                               in
                                               match iter (s,s_values) with
                                                 | Some id -> TStrct(s_name)
                                                 | None -> buildStruct l )
                 in
                 buildStruct prog.types

                                    
    
    (* once Strct typechecking is done these will make more sense *)
    | GetF(e,f) -> assert false
    | SetF(e, f, e') -> assert false
    (* Fix Point *)
    | Fix(f, t, e) -> assert false 
    (* Other cases *)
    | Seq(e, e') -> let _ = type_expr e tenv in
                     type_expr e' tenv;
  in

  type_expr prog.code TypEnv.empty
