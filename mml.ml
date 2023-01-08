(* Abstract Syntax for Mini-ML *)

type typ = 
  | TInt 
  | TBool
  | TUnit
  | TFun of typ * typ
  | TStrct of string
  | TMatch
  | TAnything
  | TArray of typ
  
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

type luop =
  | Len

type lbop = 
  | Concat 
  | EqL

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
  | Array of string * typ * (expr) list
  | ListUop of luop * expr
  | ListBop of lbop * expr * expr 
  | MatchPattern of expr * (expr) list
  | MatchPossibility of expr * expr 
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
