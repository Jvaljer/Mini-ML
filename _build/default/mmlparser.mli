
(* The type of tokens. *)

type token = 
  | WITH
  | TYPE
  | TUNIT
  | TINT
  | THEN
  | TBOOL
  | SEMI
  | SELECT
  | RPAR
  | REC
  | RBRACKET
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
  | MATCH
  | LT
  | LPAR
  | LET
  | LEN
  | LE
  | LBRACKET
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
  | CONCAT
  | COMMA
  | COLON
  | BOOL of (bool)
  | ASS
  | ARRAY
  | ANYTHING
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Mml.prog)
