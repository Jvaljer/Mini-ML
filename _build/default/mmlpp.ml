(* Printings handler for Mini-ML *)
open Format
open Mml

let rec typ_to_string = function
  | TInt  -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TFun(typ1, typ2) -> 
     Printf.sprintf "(%s) -> %s" (typ_to_string typ1) (typ_to_string typ2)
  | TStrct s -> s
  (* Extensions *)
  | TMatch -> "match"
  | TAnything -> "anything"
  | TArray t -> Printf.sprintf "%s[]" (typ_to_string t) 

let rec print_fields ppf = function
  | [] -> fprintf ppf ""
  | (x, t, m) :: l -> let mut = if m then "mutable " else "" in
                 fprintf ppf "%s %s: %s;@ %a" mut x (typ_to_string t) print_fields l

let rec print_types ppf = function
  | [] -> fprintf ppf "@."
  | (t, s) :: l -> fprintf ppf "type %s = { @[%a}@]@.%a" t print_fields s print_types l

let uop_to_string = function
  | Neg -> "-"
  | Not -> "not "
let bop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "mod"
  | Eq  -> "=="
  | Neq -> "!="
  | Lt  -> "<"
  | Le  -> "<="
  | And -> "&&"
  | Or  -> "||"
let luop_to_string = function
  | Len -> "len"
let lbop_to_string = function
  | Concat -> "@"

let rec print_expr ppf = function
  | Int n -> fprintf ppf "%i" n
  | Bool b -> fprintf ppf "%b" b
  | Unit -> fprintf ppf "()"
  | Uop(op, e) -> fprintf ppf "(%s %a)" (uop_to_string op) print_expr e
  | Bop(op, e1, e2) -> fprintf ppf "(@[%a %s %a)@]" print_expr e1 (bop_to_string op) print_expr e2
  | Var x -> fprintf ppf "%s" x
  | Let(x, e1, e2) -> fprintf ppf "@[(let %s =@ %a in@ %a)@]" x print_expr e1 print_expr e2
  | If(c, e1, e2) -> fprintf ppf "@[if %a then@ %a else@ %a@]" print_expr c print_expr e1 print_expr e2
  | Fun(x, t, e) -> fprintf ppf "fun (%s: %s) -> %a" x (typ_to_string t) print_expr e
  | App(e1, e2) -> fprintf ppf "(%a %a)" print_expr e1 print_expr e2
  | Fix(x, t, e) -> fprintf ppf "fix (%s: %s) = %a" x (typ_to_string t) print_expr e
  | Strct l -> fprintf ppf "{ @[%a}@]" print_defs l
  | GetF(e, x) -> fprintf ppf "(%a).%s" print_expr e x
  | SetF(e1, x, e2) -> fprintf ppf "(%a).%s <- %a" print_expr e1 x print_expr e2
  | Seq(e1, e2) -> fprintf ppf "%a; @[%a@]" print_expr e1 print_expr e2
  (* Extension *)
  | MatchPossibility(e,e') -> fprintf ppf "| %a -> %a" print_expr e print_expr e'
  | MatchPattern(e,poss) -> fprintf ppf "match %a with \n %a" print_expr e print_match poss
  | Anything -> fprintf ppf "_"
  | Array(id,t,l) -> fprintf ppf "%s:%s=%a" id (typ_to_string t) print_array l 
  | ListUop(op,id) -> fprintf ppf "%s(%a)" (luop_to_string op) print_expr id
  | ListBop(op,id,id') -> fprintf ppf "%a%s%a" print_expr id (lbop_to_string op) print_expr id'
and print_defs ppf = function
  | [] -> fprintf ppf ""
  | (x, e) :: l -> fprintf ppf "%s = %a; %a" x print_expr e print_defs l
and print_array ppf = function 
  | [] -> fprintf ppf ""
  | n::s -> fprintf ppf "%a, %a" print_expr n print_array s
and print_match ppf = function
  | [] -> fprintf ppf "\n"
  | m::s -> fprintf ppf "\n %a %a" print_expr m print_match s
let print_prog ppf prog =
  fprintf ppf "%a@.%a@." print_types prog.types print_expr prog.code

