(* Abstract Syntax for Mini-ML *)

type typ = 
  | TInt 
  | TBool
  | TUnit
  | TFun of typ * typ
  | TStrct of string
  | TArrayInt 
  | TMatch
  
type strct = (string * typ * bool) list

type uop = 
  | Neg 
  | Not

type bop = 
  | Add 
  | Sub 
  | Mul 
  | Div 
  | Mod 
  | Eq 
  | Neq 
  | Lt 
  | Le 
  | And 
  | Or

type expr =
  | Int   of int
  | Bool  of bool
  | Unit
  | Uop   of uop * expr
  | Bop   of bop * expr * expr
  | Var   of string
  | Let   of string * expr * expr
  | If    of expr * expr * expr
  | Fun   of string * typ * expr
  | App   of expr * expr
  | Fix   of string * typ * expr
  | Strct of (string * expr) list
  | GetF  of expr * string
  | SetF  of expr * string * expr
  | Seq   of expr * expr
  (* extensions *)
  | ArrayInt of string * (expr) list (* int list or expr list ??? *)
  | MatchPattern of expr * (expr * expr) list
  | Anything 

type prog = {
    types: (string * strct) list;
    code: expr;
  }

(* Usable functions to handle functions with multiple arguments *)
let rec mk_fun xs e = match xs with
  | [] -> e
  | (x, t)::xs -> Fun(x, t, mk_fun xs e)
  
let rec mk_fun_type xs t = match xs with
  | [] -> t
  | (_, t')::xs -> TFun(t', mk_fun_type xs t)
