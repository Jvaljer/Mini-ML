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
(* Heap-Values (only used for structures & functions) *)
type heap_value =
  | VClos  of string * expr * value Env.t
  | VStrct of (string, value) Hashtbl.t

(* string rending values (printing them)*)
let print_value = function
  | VInt n  -> Printf.printf "%d\n" n
  | VBool b -> Printf.printf "%b\n" b
  | VUnit   -> Printf.printf "()\n"
  | VPtr p  -> Printf.printf "@%d\n" p

(* Whole Mini-ML program interpret *)
let eval_prog (p: prog): value =
  
  (* Initializating global memory *)
  let (mem: (int, heap_value) Hashtbl.t) = Hashtbl.create 16 in

  (* function used to create new values (potentially added to the global memory)*)
  let new_ptr =
    let cpt = ref 0 in
    fun () -> incr cpt; !cpt
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
        (* Binary Operands : Arith *)
      | Bop(Add, e1, e2) -> VInt(evalInt e1 env + evalInt e2 env)
      | Bop(Mul, e1, e2) -> VInt(evalInt e1 env * evalInt e2 env)
      | Bop(Sub, e1, e2) -> VInt(evalInt e1 env - evalInt e2 env)
      | Bop(Div, e1, e2) -> VInt(evalInt e1 env / evalInt e2 env)
      | Bop(Mod, e1, e2) -> VInt(evalInt e1 env mod evalInt e2 env)
        (* Binary Operands : Boolean Arith *)
      | Bop(And, e1, e2) -> VBool(evalBool e1 env && evalBool e2 env)
      | Bop(Or, e1, e2) -> VBool(evalBool e1 env || evalBool e2 env)
      | Bop(Lt, e1, e2) -> VBool(evalInt e1 env < evalInt e2 env)
      | Bop(Le, e1, e2) -> VBool(evalInt e1 env <= evalInt e2 env)
      | Bop(Eq, e1, e2) -> VBool(evalBool e1 env == evalBool e2 env)
      | Bop(Neq, e1, e2) -> VBool(eval e1 env != eval e2 env)
      (* Conditons *)
      | If(c, e1, e2) -> if evalBool c env then eval e1 env else eval e2 env
      (* Functions *)
      | Fun(f, _, e) -> let ptr = new_ptr () in
                        Hashtbl.add mem ptr (VClos(f,e, env));
                        VPtr ptr (* trying to 'mirror' the typechecking method to the interpretation *)
      | Let(id, e1, e2) -> eval e2 (Env.add id (eval e1 env) env)
      | App(f1, f2) -> (* with Fun interpretation added -> reach for the value associated in mem *)
                       ( match eval f1 env with
                           | VPtr ptr -> ( match Hashtbl.find mem ptr with
                                            | VClos(str,expr,env) -> eval expr (Env.add str (eval f2 env) env)
                                            | _ -> assert false )
                           | _ -> assert false )
      (* Structures *)
      | Strct s -> assert false 
      | GetF(e, f) -> assert false
      | SetF(e1, f , e2) -> assert false 
      (* Fix Point *)
      | Fix(f, t, e) -> assert false (* might be similar to Fun() ? *)
      (* Sequencies *)
      | Seq(e1, e2) -> let _ = eval e1 env in 
                       eval e2 env; 



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
