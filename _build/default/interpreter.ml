(* Value Interpreter for Mini-ML language*)

open Mml

(* Map Environment -> matching values with var-names *)
module Env = Map.Make(String)

(* All possible Values (type of values)*)
type value =
  | VInt   of int
  | VBool  of bool
  | VUnit
  | VPtr   of int
  | VArrayInt  of (int) list

(* Heap-Values (only used for structures & functions) *)
type heap_value =
  | VClos  of string * expr * value Env.t
  | VStrct of (string, value) Hashtbl.t
  | VArray of string * (value) list * value Env.t

(* string rending values (printing them)*)
let print_value = function
  | VInt n  -> Printf.printf "%d\n" n
  | VBool b -> Printf.printf "%b\n" b
  | VUnit   -> Printf.printf "()\n"
  | VPtr p  -> Printf.printf "@%d\n" p
  | VArrayInt l -> let printList list = 
                     Printf.printf "[";
                     let rec printing list' =
                       match list' with
                         | [] -> Printf.printf "]"
                         | n::s -> Printf.printf "%d" n; printing s
                     in 
                     printing list
                   in
                   printList l

(* Whole Mini-ML program interpret *)
let eval_prog (p: prog): value =
  
  (* Initializating global memory *)
  let (mem: (int, heap_value) Hashtbl.t) = Hashtbl.create 16 in

  (* function used to create new values (potentially added to the global memory)*)
  let new_ptr =
    let cpt = ref 0 in
    fun () -> incr cpt; !cpt
  in

  (* auxiliar functions *)
  let findStrct s = 
      match Hashtbl.find mem s with 
        | VStrct v -> v
        | _ -> assert false 
  in
  
  let findArray ptr = 
      match Hashtbl.find mem ptr with 
        | VArray(_,l,_) -> l
        | _ -> assert false
  in

  (* Interpreting a special Expr, consideratibg the environment + memory within which we wanna interpret it *)
  let rec eval (e: expr) (env: value Env.t): value = 
    match e with
      (* simple cases *)
      | Int n  -> VInt n
      | Bool b -> VBool b
      | Unit -> VUnit 
      | Var x -> Env.find x env
      (* Arithmetic *)
        (* Unary Operands *)
      | Uop(Neg, e) -> VInt(evalInt e env)
      | Uop(Not, e) -> VBool(evalBool e env)
        (* Binary Operands : Arith *) (* Maybe possible to 'list' together ? *)
      | Bop(Add, e, e') -> VInt(evalInt e env + evalInt e' env)
      | Bop(Mul, e, e') -> VInt(evalInt e env * evalInt e' env)
      | Bop(Sub, e, e') -> VInt(evalInt e env - evalInt e' env)
      | Bop(Div, e, e') -> VInt(evalInt e env / evalInt e' env)
      | Bop(Mod, e, e') -> VInt(evalInt e env mod evalInt e' env)
        (* Binary Operands : Boolean Arith *) (* Same ? *)
      | Bop(And, e, e') -> VBool(evalBool e env && evalBool e' env)
      | Bop(Or, e, e') -> VBool(evalBool e env || evalBool e' env)
      | Bop(Lt, e, e') -> VBool(evalInt e env < evalInt e' env)
      | Bop(Le, e, e') -> VBool(evalInt e env <= evalInt e' env)
      | Bop(Eq, e, e') -> VBool(evalBool e env == evalBool e' env)
      | Bop(Neq, e, e') -> VBool(eval e env != eval e' env)
      (* Conditons *)
      | If(c, e, e') -> if evalBool c env then eval e env else eval e' env
      (* Functions *)
      | Fun(f, _, e) -> let ptr = new_ptr () in
                        Hashtbl.add mem ptr (VClos(f,e, env));
                        VPtr ptr (* trying to 'mirror' the typechecking method to the interpretation *)
      | Let(id, e, e') -> eval e' (Env.add id (eval e env) env)
      | App(f, f') -> (* with Fun interpretation added -> reach for the value associated in mem *)
                       ( match eval f env with
                           | VPtr ptr -> ( match Hashtbl.find mem ptr with
                                            | VClos(str,expr,env) -> eval expr (Env.add str (eval f' env) env)
                                            | _ -> assert false )
                           | _ -> assert false )
      (* Structures *)
      | Strct s -> (* first of all we wanna create a Hashtbl with all the struct elements *)
                   let rec storeStruct strct hash =
                     match strct with 
                       | [] -> (* now that all the struct has been evaluated and stored we add it to the memory *)
                               let ptr = new_ptr () in 
                               Hashtbl.add mem ptr (VStrct hash);
                               VPtr ptr
                     (* we wanna interpret each one of the struct's expr and add the evaluation in the Hashtbl associated with the correct id *)
                       | (id,expr)::s' -> Hashtbl.add hash id (eval expr env); storeStruct s' hash 
                   in 
                 (* now apply all above and struct is well interpreted *)
                   let structHash = Hashtbl.create (List.length s) in 
                   storeStruct s structHash
      | GetF(s, expr) -> (* first we wanna match the return value of the expr *)
                      ( match eval s env with 
                        (* if we are intepreting a structure as wanted then inteprete the given expr*)
                          | VPtr ptr -> Hashtbl.find (findStrct ptr) expr
                          | _ -> assert false )
      | SetF(e, f , e') -> (* here check before anything the given expr intepretation *)
                           let v = eval e' env in
                           (* then check if the given ident is well assigned to a structure *)
                           ( match eval e env with 
                               (* and if so find the structure and replace its actual assigned expr *)
                               | VPtr ptr -> Hashtbl.replace (findStrct ptr) f v; VUnit
                               | _ -> assert false )
                          (* Error : This variant pattern is expected to have type value
                                     There is no constructor SetF within type value *)
      (* Sequences *)
      | Seq(e, e') -> let _ = eval e env in
                      eval e' env;
      (* Fix Point *)
      | Fix(f, _, e) -> (* e shall be a Fun so we match it first *)
                        ( match e with 
                            | Fun(id, _, expr) -> (* first we need to define a new environment corresponding to the whole function's values *)
                                                  let ptr = new_ptr () in
                                                  let new_env = Env.add f (VPtr ptr) env in 
                                                  (* then we must get the value f(x) = x *)
                                                  let v = VClos(id,expr,new_env) in
                                                  (* and finally add it to the heap *)
                                                  Hashtbl.add mem ptr v;
                                                  VPtr ptr
                            | _ -> assert false ) 
      (* Integer Array *)
      | ArrayInt(_,l) -> let rec storeArray list ret =
                            ( match list with 
                                | [] -> VArrayInt ret
                                | (Int n)::s -> storeArray s (n::ret)
                                | _ -> assert false )
                          in
                          storeArray l []
      (* Matching Pattern *)
      | MatchPattern(e, l) -> let eval_e = eval e env in 
                              let rec match_expr list = 
                                ( match list with 
                                    | [] -> assert false 
                                    | MatchPossibility(e_i,e_cons)::s -> let eval_e_i = eval e_i env in 
                                                          if eval_e = eval_e_i then eval e_cons env
                                                          else match_expr s 
                                    | _ -> assert false )
                              in
                              match_expr l 
      | MatchPossibility(e,e') -> (* here we just want to make sure both expr are well evaluated *)
                                  let _ = eval e env in 
                                  eval e' env 
      | Anything -> VUnit 
      (* Uniform Arrays *)
      | Array(id,_,l) -> let rec store list r =
                          ( match (list,r) with  
                              | [],ret -> let ptr = new_ptr () in
                                          Hashtbl.add mem ptr (VArray(id,ret,env));
                                          VPtr ptr
                              | e::s,ret -> let v = eval e env in
                                            store s (ret@[v]) )
                        in
                        store l [] 
      | ListUop(Len,l_id) -> ( match eval l_id env with
                                 | VPtr ptr -> let l = findArray ptr in
                                               let rec length cpt list =
                                                 ( match list with
                                                     | [] -> VInt cpt 
                                                     | _::s -> length (cpt+1) s )
                                               in
                                               length 0 l
                                 | _ -> assert false )
                             

  (* Interpreting the Expr when it's supposed to be an Integer *)
  and evalInt (e: expr) (env: value Env.t): int = 
    match eval e env with
      | VInt n -> n
      | _ -> assert false
  (* Interpreting the Expr when it's supposed to be a Boolean *)
  and evalBool (e: expr) (env: value Env.t): bool =
    match eval e env with 
      | VBool b -> b
      | _ -> assert false 
  in

  eval p.code Env.empty
