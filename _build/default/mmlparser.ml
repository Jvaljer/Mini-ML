
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
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
    | IDENT of (
# 12 "mmlparser.mly"
       (string)
# 42 "mmlparser.ml"
  )
    | FUN
    | EQ
    | EOF
    | ELSE
    | DOT
    | DIV
    | CST of (
# 10 "mmlparser.mly"
       (int)
# 53 "mmlparser.ml"
  )
    | COLON
    | BOOL of (
# 11 "mmlparser.mly"
       (bool)
# 59 "mmlparser.ml"
  )
    | ASS
    | AND
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState113
  | MenhirState110
  | MenhirState107
  | MenhirState104
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState97
  | MenhirState95
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState86
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState46
  | MenhirState44
  | MenhirState40
  | MenhirState37
  | MenhirState34
  | MenhirState32
  | MenhirState31
  | MenhirState30
  | MenhirState29
  | MenhirState27
  | MenhirState24
  | MenhirState17
  | MenhirState13
  | MenhirState9
  | MenhirState6
  | MenhirState4
  | MenhirState0

# 2 "mmlparser.mly"
  
  (*open Lexing*)
  open Mml


# 149 "mmlparser.ml"

let rec _menhir_goto_nonempty_list_id_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Mml.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (id_list : ((string * Mml.expr) list))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 82 "mmlparser.mly"
             ( Strct(id_list) )
# 168 "mmlparser.ml"
             in
            _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.expr))), _, (xs : ((string * Mml.expr) list))) = _menhir_stack in
        let _v : ((string * Mml.expr) list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 184 "mmlparser.ml"
         in
        _menhir_goto_nonempty_list_id_def_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_goto_nonempty_list_type_def_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.strct) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.typ * bool))), _, (xs : (Mml.strct))) = _menhir_stack in
        let _v : (Mml.strct) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 597 "mmlparser.ml"
         in
        _menhir_goto_nonempty_list_type_def_arg_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (t_name : (
# 12 "mmlparser.mly"
       (string)
# 612 "mmlparser.ml"
            ))), _, (t_def : (Mml.strct))) = _menhir_stack in
            let _v : (string * Mml.strct) = 
# 53 "mmlparser.mly"
    ( (t_name,t_def) )
# 617 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | BOOL _ | CST _ | FUN | IDENT _ | IF | LBRACE | LET | LPAR | NEG | NOT | PARS ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LPAR ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TBOOL ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TUNIT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_goto_list_fun_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Mml.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | LPAR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | TBOOL ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | TUNIT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.typ))), _, (xs : ((string * Mml.typ) list))) = _menhir_stack in
        let _v : ((string * Mml.typ) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 702 "mmlparser.ml"
         in
        _menhir_goto_list_fun_arg_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | CST _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | FUN ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (se : (Mml.expr))), (f : (
# 12 "mmlparser.mly"
       (string)
# 817 "mmlparser.ml"
            ))), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 111 "mmlparser.mly"
                              ( SetF(se, f, e) )
# 822 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState104 | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 112 "mmlparser.mly"
                              ( Seq(e1, e2) )
# 876 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 126 "mmlparser.mly"
          ( Add )
# 930 "mmlparser.ml"
             in
            
# 95 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 935 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 136 "mmlparser.mly"
          ( Or )
# 989 "mmlparser.ml"
             in
            
# 95 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 994 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 132 "mmlparser.mly"
          ( Neq )
# 1048 "mmlparser.ml"
             in
            
# 95 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1053 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 127 "mmlparser.mly"
          ( Mul )
# 1107 "mmlparser.ml"
             in
            
# 95 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1112 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 130 "mmlparser.mly"
          ( Mod )
# 1166 "mmlparser.ml"
             in
            
# 95 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1171 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 128 "mmlparser.mly"
          ( Sub )
# 1225 "mmlparser.ml"
             in
            
# 95 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1230 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 133 "mmlparser.mly"
          ( Lt )
# 1284 "mmlparser.ml"
             in
            
# 95 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1289 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 134 "mmlparser.mly"
          ( Le )
# 1343 "mmlparser.ml"
             in
            
# 95 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1348 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 131 "mmlparser.mly"
          ( Eq )
# 1402 "mmlparser.ml"
             in
            
# 95 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1407 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 129 "mmlparser.mly"
          ( Div )
# 1461 "mmlparser.ml"
             in
            
# 95 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1466 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 135 "mmlparser.mly"
          ( And )
# 1520 "mmlparser.ml"
             in
            
# 95 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1525 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (f : (string * Mml.typ))), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 102 "mmlparser.mly"
                                ( Fun(fst(f),snd(f),e) )
# 1579 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState87 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | CST _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | FUN ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState89 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | CST _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | FUN ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (c : (Mml.expr))), _), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 98 "mmlparser.mly"
                              ( If(c,e,Unit) )
# 1744 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (c : (Mml.expr))), _), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 100 "mmlparser.mly"
                              ( If(c,e1,e2) )
# 1798 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState92 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | CST _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | FUN ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 1929 "mmlparser.ml"
            ))), _, (args : ((string * Mml.typ) list))), _, (t : (Mml.typ))), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 107 "mmlparser.mly"
                              ( let f = mk_fun args e1 in
                                let fix = Fix(id, t, f) in
                                Let(id, fix, e2) )
# 1936 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState100 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | CST _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | FUN ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 2067 "mmlparser.ml"
            ))), _, (args : ((string * Mml.typ) list))), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 104 "mmlparser.mly"
                              ( let f = mk_fun args e1 in
                                Let(id, f , e2) )
# 2073 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState103 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | CST _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | FUN ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (id : (
# 12 "mmlparser.mly"
       (string)
# 2153 "mmlparser.ml"
                ))), _, (e : (Mml.expr))), _) = _menhir_stack in
                let _v : (string * Mml.expr) = 
# 90 "mmlparser.mly"
    ( (id,e) )
# 2158 "mmlparser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                | RBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (x : (string * Mml.expr))) = _menhir_stack in
                    let _v : ((string * Mml.expr) list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 2173 "mmlparser.ml"
                     in
                    _menhir_goto_nonempty_list_id_def_ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState110 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 84 "mmlparser.mly"
             ( e )
# 2236 "mmlparser.ml"
             in
            _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState113 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (types : ((string * Mml.strct) list))), _, (code : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.prog) = 
# 47 "mmlparser.mly"
    ( {types ; code} )
# 2266 "mmlparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Mml.prog)) = _v in
            Obj.magic _1
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MUL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | _ ->
        _menhir_fail ()

and _menhir_reduce40 : _menhir_env -> (('ttv_tail * _menhir_state * (Mml.expr))) * (
# 12 "mmlparser.mly"
       (string)
# 2310 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (se : (Mml.expr))), (f : (
# 12 "mmlparser.mly"
       (string)
# 2316 "mmlparser.ml"
    ))) = _menhir_stack in
    let _v : (Mml.expr) = 
# 80 "mmlparser.mly"
             ( GetF(se, f) )
# 2321 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Mml.typ))) = _menhir_stack in
            let _v : (Mml.typ) = 
# 70 "mmlparser.mly"
             ( t )
# 2344 "mmlparser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | ASS | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Mml.typ))), _, (t2 : (Mml.typ))) = _menhir_stack in
            let _v : (Mml.typ) = 
# 68 "mmlparser.mly"
             ( TFun(t1, t2) )
# 2366 "mmlparser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (mut : (unit option))), (id : (
# 12 "mmlparser.mly"
       (string)
# 2389 "mmlparser.ml"
            ))), _, (t : (Mml.typ))) = _menhir_stack in
            let _v : (string * Mml.typ * bool) = 
# 59 "mmlparser.mly"
    ( if mut!=None then (id,t,true) else (id,t,false) )
# 2394 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | MUTABLE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | IDENT _ ->
                _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Mml.typ * bool))) = _menhir_stack in
                let _v : (Mml.strct) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 2411 "mmlparser.ml"
                 in
                _menhir_goto_nonempty_list_type_def_arg_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 2438 "mmlparser.ml"
            ))), _, (t : (Mml.typ))) = _menhir_stack in
            let _v : (string * Mml.typ) = 
# 118 "mmlparser.mly"
    ( (id,t) )
# 2443 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState49 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RARROW ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BOOL _v ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                    | CST _v ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                    | FUN ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | IDENT _v ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                    | IF ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | LET ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | LPAR ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | NEG ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | NOT ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | PARS ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState97 | MenhirState95 | MenhirState37 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAR ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | ASS | COLON ->
                    _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | CST _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | FUN ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
        | RARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Mml.typ) list) = 
# 211 "<standard.mly>"
    ( [] )
# 2562 "mmlparser.ml"
     in
    _menhir_goto_list_fun_arg_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "mmlparser.mly"
       (string)
# 2569 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | FUN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | IF ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | NEG ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | NOT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | LPAR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | TBOOL ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | TUNIT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_s_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState27 | MenhirState30 | MenhirState104 | MenhirState34 | MenhirState101 | MenhirState99 | MenhirState93 | MenhirState46 | MenhirState90 | MenhirState88 | MenhirState47 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LARROW ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BOOL _v ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
                    | CST _v ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
                    | FUN ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | IDENT _v ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
                    | IF ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | LET ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | LPAR ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | NEG ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | NOT ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | PARS ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
                | AND | BOOL _ | CST _ | DIV | DOT | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | SEMI | THEN ->
                    _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 94 "mmlparser.mly"
                              ( se )
# 2729 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState113 | MenhirState110 | MenhirState103 | MenhirState100 | MenhirState102 | MenhirState92 | MenhirState94 | MenhirState87 | MenhirState89 | MenhirState91 | MenhirState86 | MenhirState58 | MenhirState60 | MenhirState62 | MenhirState64 | MenhirState66 | MenhirState68 | MenhirState70 | MenhirState72 | MenhirState74 | MenhirState76 | MenhirState78 | MenhirState80 | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Mml.expr))), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 97 "mmlparser.mly"
                              ( App(e, se) )
# 2751 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 122 "mmlparser.mly"
        ( Neg )
# 2773 "mmlparser.ml"
             in
            
# 96 "mmlparser.mly"
                              ( Uop(op, se) )
# 2778 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 121 "mmlparser.mly"
        ( Not )
# 2800 "mmlparser.ml"
             in
            
# 96 "mmlparser.mly"
                              ( Uop(op, se) )
# 2805 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.typ) = 
# 65 "mmlparser.mly"
             ( TUnit )
# 2824 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.typ) = 
# 63 "mmlparser.mly"
             ( TInt )
# 2835 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.typ) = 
# 64 "mmlparser.mly"
             ( TBool )
# 2846 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LPAR ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TBOOL ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TUNIT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "mmlparser.mly"
       (string)
# 2874 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (
# 12 "mmlparser.mly"
       (string)
# 2882 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.typ) = 
# 66 "mmlparser.mly"
             ( TStrct(id) )
# 2887 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.expr) = 
# 77 "mmlparser.mly"
             ( Unit )
# 2903 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | ASS ->
            _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | REC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAR ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | COLON ->
                _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CST _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | FUN ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "mmlparser.mly"
       (string)
# 3087 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 12 "mmlparser.mly"
       (string)
# 3095 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 78 "mmlparser.mly"
            ( Var(x) )
# 3100 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "mmlparser.mly"
       (int)
# 3120 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 10 "mmlparser.mly"
       (int)
# 3128 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 75 "mmlparser.mly"
             ( Int(n) )
# 3133 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "mmlparser.mly"
       (bool)
# 3140 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 11 "mmlparser.mly"
       (bool)
# 3148 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 76 "mmlparser.mly"
             ( Bool(b) )
# 3153 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_MUTABLE_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | LPAR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | TBOOL ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | TUNIT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_type_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Mml.strct) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.strct))), _, (xs : ((string * Mml.strct) list))) = _menhir_stack in
        let _v : ((string * Mml.strct) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3213 "mmlparser.ml"
         in
        _menhir_goto_list_type_def_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | CST _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | FUN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | IF ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | NEG ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | NOT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
    | _ ->
        _menhir_fail ()

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) = 
# 114 "<standard.mly>"
    ( None )
# 3255 "mmlparser.ml"
     in
    _menhir_goto_option_MUTABLE_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) = 
# 116 "<standard.mly>"
    ( Some x )
# 3267 "mmlparser.ml"
     in
    _menhir_goto_option_MUTABLE_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Mml.strct) list) = 
# 211 "<standard.mly>"
    ( [] )
# 3543 "mmlparser.ml"
     in
    _menhir_goto_list_type_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | MUTABLE ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
                | IDENT _ ->
                    _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState4
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Mml.prog) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BOOL _ | CST _ | FUN | IDENT _ | IF | LBRACE | LET | LPAR | NEG | NOT | PARS ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3632 "mmlparser.ml"
