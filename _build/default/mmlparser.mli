
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
  | LT
  | LPAR
  | LET
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
  | COMMA
  | COLON
  | BOOL of (bool)
  | ASS
  | ARRAY
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Mml.prog)
