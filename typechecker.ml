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
    | [] -> error (Printf.sprintf "struct %s doesn't exists" strct_id)
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
                        | t_err -> error (Printf.sprintf "the type of this expr is %s, it's not a function" (Mmlpp.typ_to_string t_err)) ) 
    (* Structures *)
    | Strct s -> let rec checkStruct = function
                   (* if the prog types are empty then print it *)
                   | [] -> error (Printf.sprintf "struct hasn't led to make a TStrct")
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
                                         Not_found -> error (Printf.sprintf "%s isn't a struct (not in the list...)" f )) 
                       | t_err ->  error (Printf.sprintf "expr is typed as %s but not as a struct" (Mmlpp.typ_to_string t_err)) )
    | SetF(e, f, e') -> (* same as GetF but with specifications -> we wanna check the second expr too & add the 'mutable' possibility *)
                        ( match type_expr e tenv with
                            | TStrct strct -> ( let s = findStruct strct prog.types tenv in
                                                  try 
                                                    let _,t,mut = (List.find (fun(f',_,_) -> f' = f)) s in 
                                                    check e' t tenv; 
                                                    if mut 
                                                      then TUnit 
                                                    else 
                                                      error (Printf.sprintf "%s.%s isn't a mutable" strct f ) 
                                                  with 
                                                    Not_found -> error (Printf.sprintf "%s isn't in the struct" f ) )
                            | t_err -> error (Printf.sprintf "the expr isn't matching TStrct, it's typed as %s" (Mmlpp.typ_to_string t_err)) )
    (* Sequence *)
    | Seq(e, e') -> let _ = type_expr e tenv in
                    type_expr e' tenv; 
    (* Fix Point *)
    | Fix(f, t, e) -> let env = TypEnv.add f t tenv in
                      type_expr e env (* here we just check if inside e restrained environment (which expr belongs to) expr has the right type *)
    (* Matching Pattern *)
    | MatchPattern(e, l) -> (* here, we want to test if in the possibilities list there's AT LEAST on 'Anything' or one 'same typed' expr *)
                            let type_e = type_expr e tenv in 
                            let rec test_type_expr = function
                              | [] -> error (Printf.sprintf "there isn't any possible match in this pattern" ) 
                              | m::s -> ( match m with 
                                            | MatchPossibility(_,_) -> (* here we wanna say 'OK' if e_i is typed as TAnything or type_e *)
                                                                          ( match type_expr m tenv with (* for that we use the type_expr of m which returns the e_i type *)
                                                                              | TAnything -> TMatch 
                                                                              | type_m -> if type_e=type_m then TMatch
                                                                                          else test_type_expr s )
                                            | _ -> error (Printf.sprintf "the wanted match doesn't belongs in our pattern") )
                            in
                            test_type_expr l
    | MatchPossibility(e,e') -> (* for each possibility we want it to typecheck the consequence & return the type of the matchign expr *)
                                let _ = type_expr e' tenv in
                                type_expr e tenv
    | Anything -> TAnything
    (* Uniform Arrays *)
    | Array(_,t,l) -> let rec arrayTypeCheck = function
                         | [] -> TArray t
                         | e::s -> let t' = type_expr e tenv in 
                                   if t<>t' then 
                                     error (Printf.sprintf "array is typed as %s but expr is typed as %s" (Mmlpp.typ_to_string t) (Mmlpp.typ_to_string t') )
                                   else 
                                     arrayTypeCheck s
                       in
                       arrayTypeCheck l 
    | ListUop(Len,l) -> ( match type_expr l tenv with 
                            | TArray _ -> TInt 
                            | t_err -> error (Printf.sprintf "expr is not a list, typed as %s" (Mmlpp.typ_to_string t_err)) )
    | ListBop(Concat,l,l') -> ( match type_expr l tenv, type_expr l' tenv with 
                                  | TArray t, TArray t' -> if t<>t' then 
                                                             error (Printf.sprintf "both list aren't same typed -> %s <> %s" (Mmlpp.typ_to_string t) (Mmlpp.typ_to_string t'))
                                                           else
                                                             TArray t 
                                  | t_err,t_err' -> error (Printf.sprintf "at least one of the 2 list isn't well Array typed : %s %s" (Mmlpp.typ_to_string t_err) (Mmlpp.typ_to_string t_err')) )
  in

  type_expr prog.code TypEnv.empty