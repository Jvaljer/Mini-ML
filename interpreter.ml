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
    | Int n  -> VInt n
    | Bop(Add, e1, e2) -> VInt (evali e1 env + evali e2 env)
    | Bop(Mul, e1, e2) -> VInt (evali e1 env * evali e2 env)

  (* Interpreting the Expr when it's supposed to be an Integer *)
  and evali (e: expr) (env: value Env.t): int = 
    match eval e env with
    | VInt n -> n
    | _ -> assert false
  in

  eval p.code Env.empty
