(* Mini-ML Lexer *)
{
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
        "array", ARRAY;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)* (*an ident can be define with digits & alphas*)
let keyword = ['a'-'z']+ (*none of our keywords as any digit in it*)
  
rule token = parse
  (*first let's give all the pagination symbols*)
  | ['\n']           { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+ { token lexbuf }
  | "(*"             { comment lexbuf; token lexbuf }
  (*and all word we could possibly want to save*)
  | number as n      { CST(int_of_string n) }
  | ident as id      { IDENT(id) }
  | keyword as key   { keyword_or_ident key }
  (*All Arithmetic Symbols*)
  (*Binary Operand*)
    (*'Numeral' Arithmetic*)
  | "+" { PLUS }
  | "*" { MUL }
  | "-" { MINUS }
  | "/" { DIV }
    (*Boolean Arithmetic*)
  | "!=" { NEQ }
  | "==" { EQ }
  | "<"  { LT }
  | "<=" { LE }
  | "&&" { AND }
  | "||" { OR }
  (*Unary Operand*)
  | "-"  { NEG }
  (*Syntax symbols*)
  | "."  { DOT }
  | ":"  { COLON }
  | ";"  { SEMI }
  | "->" { LARROW }
  | "<-" { RARROW }
  | "="  { ASS } (*for 'association' whih differs from 'equality' ==*)
  | "()" { PARS } (*represent 'unit'*)
  (*Parenthesis*)
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { LBRACE }
  | "}"  { RBRACE }
  (*arrays extension*)
  | "["  { LBRACKET }
  | "]"  { RBRACKET }
  | ","  { COMMA }
  (*any other word/character + EOF*)
  | _    { raise (Lexing_error ("unknown character : " ^ (lexeme lexbuf))) }
  | eof  { EOF }

and comment = parse
  | "*)" { () }
  | "(*" { comment lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexing_error "unterminated comment") }
