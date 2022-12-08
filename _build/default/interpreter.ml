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
      | Bop(Add, e, e') -> VInt(evalInt e env + evalInt e' env)
      | Bop(Mul, e, e') -> VInt(evalInt e env * evalInt e' env)
      | Bop(Sub, e, e') -> VInt(evalInt e env - evalInt e' env)
      | Bop(Div, e, e') -> VInt(evalInt e env / evalInt e' env)
      | Bop(Mod, e, e') -> VInt(evalInt e env mod evalInt e' env)
        (* Binary Operands : Boolean Arith *)
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
      | Strct s -> assert false 
      | GetF(e, f) -> assert false
      | SetF(e, f , e') -> assert false 
      (* Fix Point *)
      | Fix(f, t, e) -> assert false (* might be similar to Fun() ? *)
      (* Sequencies *)
      | Seq(e, e') -> let _ = eval e env in 
                       eval e' env; 



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
