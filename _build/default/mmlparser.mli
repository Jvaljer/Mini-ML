
(* The type of tokens. *)

type token = 
  | TYPE
  | TUNIT
  | TINT
  | THEN
  | TBOOL
  | SEMI
  | RPAR
  | REC
  | RBRACE
  | RARROW
  | PLUS
  | PARS
  | OR
  | NOT
  | NEQ
  | NEG
  | MUTABLE
  | MUL
  | MOD
  | MINUS
  | LT
  | LPAR
  | LET
  | LE
  | LBRACE
  | LARROW
  | IN
  | IF
  | IDENT of (string)
  | FUN
  | EQ
  | EOF
  | ELSE
  | DOT
  | DIV
  | CST of (int)
  | COLON
  | BOOL of (bool)
  | ASS
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Mml.prog)
