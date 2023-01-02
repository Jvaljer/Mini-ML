# 2 "mmllexer.mll"
 
  open Lexing
  open Mmlparser

  exception Lexing_error of string

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    (*list of al lthe words which will generate a special tokens*)
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ (*conditions*)
        "if", IF;
        "then", THEN; 
        "else", ELSE;
        (*functions*)
        "fun", FUN;
        "let", LET;
        "rec", REC;
        "in", IN;
        (*all types*)
        "int", TINT;
        "bool", TBOOL;
        "unit", TUNIT;
        "mutable", MUTABLE;
        "type", TYPE; (*might be used in type definition for example*)
        (*specials arith operand*)
        "mod", MOD;
        "not", NOT;
        (*boolean values*)
        "true", BOOL(true);
        "false", BOOL(false);
        "rev", REV;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
        

# 41 "mmllexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\224\255\225\255\226\255\227\255\228\255\229\255\230\255\
    \236\255\237\255\238\255\002\000\001\000\017\000\002\000\003\000\
    \246\255\003\000\248\255\249\255\079\000\160\000\018\000\035\000\
    \002\000\255\255\232\255\253\255\235\255\245\255\244\255\234\255\
    \242\255\241\255\240\255\039\000\252\255\253\255\002\000\039\000\
    \255\255\254\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\030\000\030\000\012\000\022\000\030\000\
    \255\255\008\000\255\255\255\255\004\000\004\000\003\000\024\000\
    \001\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\002\000\002\000\
    \255\255\255\255";
  Lexing.lex_default =
   "\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\037\000\000\000\000\000\255\255\255\255\
    \000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\024\000\025\000\024\000\000\000\024\000\000\000\024\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \024\000\015\000\024\000\000\000\000\000\000\000\012\000\033\000\
    \023\000\007\000\018\000\019\000\041\000\017\000\010\000\016\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\009\000\008\000\013\000\014\000\031\000\030\000\
    \029\000\028\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\026\000\027\000\032\000\038\000\
    \040\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\004\000\000\000\003\000\000\000\020\000\
    \000\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\006\000\011\000\005\000\034\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\000\000\000\000\000\000\000\000\020\000\000\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\000\000\000\000\000\000\000\000\020\000\
    \001\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\024\000\255\255\000\000\255\255\024\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\024\000\255\255\255\255\255\255\000\000\012\000\
    \000\000\000\000\000\000\000\000\038\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\013\000\014\000\
    \015\000\017\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\023\000\023\000\013\000\035\000\
    \039\000\035\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\011\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\255\255\255\255\255\255\255\255\020\000\255\255\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\255\255\255\255\255\255\255\255\021\000\
    \000\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\035\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 49 "mmllexer.mll"
                     ( new_line lexbuf; token lexbuf )
# 193 "mmllexer.ml"

  | 1 ->
# 50 "mmllexer.mll"
                     ( token lexbuf )
# 198 "mmllexer.ml"

  | 2 ->
# 51 "mmllexer.mll"
                     ( comment lexbuf; token lexbuf )
# 203 "mmllexer.ml"

  | 3 ->
let
# 53 "mmllexer.mll"
              n
# 209 "mmllexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 53 "mmllexer.mll"
                     ( CST(int_of_string n) )
# 213 "mmllexer.ml"

  | 4 ->
let
# 54 "mmllexer.mll"
             id
# 219 "mmllexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 54 "mmllexer.mll"
                     ( IDENT(id) )
# 223 "mmllexer.ml"

  | 5 ->
let
# 55 "mmllexer.mll"
               key
# 229 "mmllexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 55 "mmllexer.mll"
                     ( keyword_or_ident key )
# 233 "mmllexer.ml"

  | 6 ->
# 59 "mmllexer.mll"
        ( PLUS )
# 238 "mmllexer.ml"

  | 7 ->
# 60 "mmllexer.mll"
        ( MUL )
# 243 "mmllexer.ml"

  | 8 ->
# 61 "mmllexer.mll"
        ( MINUS )
# 248 "mmllexer.ml"

  | 9 ->
# 62 "mmllexer.mll"
        ( DIV )
# 253 "mmllexer.ml"

  | 10 ->
# 64 "mmllexer.mll"
         ( NEQ )
# 258 "mmllexer.ml"

  | 11 ->
# 65 "mmllexer.mll"
         ( EQ )
# 263 "mmllexer.ml"

  | 12 ->
# 66 "mmllexer.mll"
         ( LT )
# 268 "mmllexer.ml"

  | 13 ->
# 67 "mmllexer.mll"
         ( LE )
# 273 "mmllexer.ml"

  | 14 ->
# 68 "mmllexer.mll"
         ( AND )
# 278 "mmllexer.ml"

  | 15 ->
# 69 "mmllexer.mll"
         ( OR )
# 283 "mmllexer.ml"

  | 16 ->
# 71 "mmllexer.mll"
         ( NEG )
# 288 "mmllexer.ml"

  | 17 ->
# 73 "mmllexer.mll"
         ( DOT )
# 293 "mmllexer.ml"

  | 18 ->
# 74 "mmllexer.mll"
         ( COLON )
# 298 "mmllexer.ml"

  | 19 ->
# 75 "mmllexer.mll"
         ( SEMI )
# 303 "mmllexer.ml"

  | 20 ->
# 76 "mmllexer.mll"
         ( LARROW )
# 308 "mmllexer.ml"

  | 21 ->
# 77 "mmllexer.mll"
         ( RARROW )
# 313 "mmllexer.ml"

  | 22 ->
# 78 "mmllexer.mll"
         ( ASS )
# 318 "mmllexer.ml"

  | 23 ->
# 79 "mmllexer.mll"
         ( PARS )
# 323 "mmllexer.ml"

  | 24 ->
# 81 "mmllexer.mll"
         ( LPAR )
# 328 "mmllexer.ml"

  | 25 ->
# 82 "mmllexer.mll"
         ( RPAR )
# 333 "mmllexer.ml"

  | 26 ->
# 83 "mmllexer.mll"
         ( LBRACE )
# 338 "mmllexer.ml"

  | 27 ->
# 84 "mmllexer.mll"
         ( RBRACE )
# 343 "mmllexer.ml"

  | 28 ->
# 86 "mmllexer.mll"
         ( LBRACKET )
# 348 "mmllexer.ml"

  | 29 ->
# 87 "mmllexer.mll"
         ( RBRACKET )
# 353 "mmllexer.ml"

  | 30 ->
# 89 "mmllexer.mll"
         ( raise (Lexing_error ("unknown character : " ^ (lexeme lexbuf))) )
# 358 "mmllexer.ml"

  | 31 ->
# 90 "mmllexer.mll"
         ( EOF )
# 363 "mmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 35
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 93 "mmllexer.mll"
         ( () )
# 375 "mmllexer.ml"

  | 1 ->
# 94 "mmllexer.mll"
         ( comment lexbuf; comment lexbuf )
# 380 "mmllexer.ml"

  | 2 ->
# 95 "mmllexer.mll"
         ( comment lexbuf )
# 385 "mmllexer.ml"

  | 3 ->
# 96 "mmllexer.mll"
         ( raise (Lexing_error "unterminated comment") )
# 390 "mmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

;;

