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


(* function used to find a special struct with its ident and return the associated struct *)
let rec findStruct strct_id list env = 
  (* with this function we wanna found the associated struct to teh struct_name 'strct_id' *)
  match list with 
    | [] -> assert false
    | (id,values)::strct -> if id=strct_id 
                              then values 
                            else 
                              findStruct strct_id strct env
                            
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
    | Strct s -> let rec checkStruct = function
                   (* if the prog types are empty then nothing *)
                   | [] -> assert false 
                   (* else we are gonna test if the expr given to the id is well typed *)
                   | (s_name,s_values)::l -> ( let rec typeCheckStruct = function
                                                 (* if both are empty that's because we went through well -> all is well typed so just return the struct as a whole TypeStruct*)
                                                 | [],[] -> Some s_name
                                                 (* else match it with the referenced expr in the prog.types and check if it's coherent*)
                                                 | (id,expr)::l, (id',t',_)::l' -> if id=id' then 
                                                                                    (* if the ids are matching then check types *)
                                                                                    if t'<> type_expr expr tenv then None (* if not same type return None *)
                                                                                    else typeCheckStruct (l,l') (* if same type then go to the next associations *)
                                                                                  else None (* if different ids then None *)
                                                 | _, _ -> None
                                               in
                                               (* we are gonna check if the wanted association is possible *)
                                               match typeCheckStruct (s,s_values) with
                                                 (* if all went good then make the struct a Type *)
                                                 | Some returnedName -> TStrct returnedName 
                                                 | None -> checkStruct l )
                 in
                 checkStruct prog.types

    (* once Strct typechecking is done these will make more sense *)
    | GetF(e,f) -> ( match type_expr e tenv with 
                       | TStrct strct -> (* we wanna find the associate struct and eval its type *)
                                      (let s = findStruct strct prog.types tenv in
                                         try 
                                           (* now that we've get the special associated struct we wanna get the type and evaluate the initial expr *)
                                           let _,t,_ = (List.find (fun(x,_,_) -> x = f )) s in
                                           check e t tenv;
                                           TUnit
                                       with 
                                         Not_found -> assert false )   
                       | _ -> assert false )
    | SetF(e, f, e') -> (* same as GetF but with specifications -> we wanna check the second expr too & add the 'mutable' possibility *)
                        ( match type_expr e tenv with
                            | TStrct strct -> ( let s = findStruct strct prog.types tenv in
                                                  try 
                                                    let _,t,mut = (List.find (fun(f',_,_) -> f' = f)) s in 
                                                    check e' t tenv; 
                                                    if mut 
                                                      then TUnit 
                                                    else 
                                                      assert false 
                                                  with 
                                                    Not_found -> assert false )
                            | _ -> assert false )
    (* Sequence *)
    | Seq(e, e') -> let _ = type_expr e tenv in
                     type_expr e' tenv; 
    (* Fix Point *)
    | Fix(f, t, e) -> let env = TypEnv.add f t tenv in
                      type_expr e env (* here we just check if inside e restrained environment (which expr belongs to) expr has the right type *)
    (* Integer Arrays *)
    | ArrayInt(_,l) -> (* we must check if each element of the list is well an integer and if we are getting to the end then returns the Array Type *)
                        let rec arrayTypeCheck = function 
                          | [] -> TArrayInt
                          | n::s -> (* for each element we wanna chekc if it's well an integer, and then check the other elements *)
                                    check n TInt tenv;
                                    arrayTypeCheck s
                        in
                        arrayTypeCheck l
    (* Matching Pattern *)
    | MatchPattern(e, l) -> (* what we wanna test, is if all first eleme of l's tuple has same type with e *)
                            let type_e = type_expr e tenv in 
                            let rec test_type_expr = function
                              | [] -> TMatch 
                              | m::s -> ( match m with 
                                            | MatchPossibility(_,_) -> let type_ei = type_expr m tenv in
                                                                       if type_ei <> type_e then assert false 
                                                                       else 
                                                                         test_type_expr s 
                                            | _ -> assert false )
                            in
                            test_type_expr l
    | MatchPossibility(e,e') -> (* for each possibility we want it to typecheck the consequence & return the type of the matchign expr *)
                                let _ = type_expr e' tenv in
                                type_expr e tenv
    | Anything -> TUnit
  in

  type_expr prog.code TypEnv.empty