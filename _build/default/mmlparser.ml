
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
    | REV
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
    | IDENT of (
# 12 "mmlparser.mly"
       (string)
# 45 "mmlparser.ml"
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
# 56 "mmlparser.ml"
  )
    | COLON
    | BOOL of (
# 11 "mmlparser.mly"
       (bool)
# 62 "mmlparser.ml"
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
  | MenhirState119
  | MenhirState116
  | MenhirState113
  | MenhirState110
  | MenhirState109
  | MenhirState108
  | MenhirState107
  | MenhirState106
  | MenhirState105
  | MenhirState103
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState98
  | MenhirState97
  | MenhirState96
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
  | MenhirState85
  | MenhirState84
  | MenhirState83
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
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState57
  | MenhirState55
  | MenhirState53
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


# 154 "mmlparser.ml"

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
# 108 "mmlparser.mly"
             ( Strct(id_list) )
# 173 "mmlparser.ml"
             in
            _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.expr))), _, (xs : ((string * Mml.expr) list))) = _menhir_stack in
        let _v : ((string * Mml.expr) list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 189 "mmlparser.ml"
         in
        _menhir_goto_nonempty_list_id_def_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65
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

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

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
# 626 "mmlparser.ml"
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
# 641 "mmlparser.ml"
            ))), _, (t_def : (Mml.strct))) = _menhir_stack in
            let _v : (string * Mml.strct) = 
# 79 "mmlparser.mly"
    ( (t_name,t_def) )
# 646 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | BOOL _ | CST _ | FUN | IDENT _ | IF | LBRACE | LBRACKET | LET | LPAR | NEG | NOT | PARS ->
                _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState24
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
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.typ))), _, (xs : ((string * Mml.typ) list))) = _menhir_stack in
        let _v : ((string * Mml.typ) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 731 "mmlparser.ml"
         in
        _menhir_goto_list_fun_arg_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState103 ->
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
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | CST _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | FUN ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | IF ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LBRACKET ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_list_elem_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Mml.expr))), _, (xs : (Mml.expr list))) = _menhir_stack in
        let _v : (Mml.expr list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 792 "mmlparser.ml"
         in
        _menhir_goto_list_list_elem_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (elems : (Mml.expr list))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 140 "mmlparser.mly"
                              ( IntList(elems) )
# 808 "mmlparser.ml"
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

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | ELSE | EOF | IN | LBRACE | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (se : (Mml.expr))), (f : (
# 12 "mmlparser.mly"
       (string)
# 885 "mmlparser.ml"
            ))), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 137 "mmlparser.mly"
                              ( SetF(se, f, e) )
# 890 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState110 | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 138 "mmlparser.mly"
                              ( Seq(e1, e2) )
# 910 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | BOOL _ | CST _ | ELSE | EOF | IDENT _ | IN | LBRACE | MINUS | PARS | PLUS | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 157 "mmlparser.mly"
          ( Add )
# 950 "mmlparser.ml"
             in
            
# 121 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 955 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 167 "mmlparser.mly"
          ( Or )
# 979 "mmlparser.ml"
             in
            
# 121 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 984 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 166 "mmlparser.mly"
          ( And )
# 1006 "mmlparser.ml"
             in
            
# 121 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1011 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | MINUS | MUL | NEQ | PARS | PLUS | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 163 "mmlparser.mly"
          ( Neq )
# 1043 "mmlparser.ml"
             in
            
# 121 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1048 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LT | MINUS | MOD | MUL | NEQ | PARS | PLUS | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 161 "mmlparser.mly"
          ( Mod )
# 1074 "mmlparser.ml"
             in
            
# 121 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1079 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LT | MINUS | MUL | NEQ | PARS | PLUS | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 164 "mmlparser.mly"
          ( Lt )
# 1107 "mmlparser.ml"
             in
            
# 121 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1112 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LT | MINUS | MUL | NEQ | PARS | PLUS | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 165 "mmlparser.mly"
          ( Le )
# 1140 "mmlparser.ml"
             in
            
# 121 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1145 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | BOOL _ | CST _ | DIV | ELSE | EOF | IDENT _ | IN | LBRACE | MINUS | MUL | PARS | PLUS | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 158 "mmlparser.mly"
          ( Mul )
# 1181 "mmlparser.ml"
             in
            
# 121 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1186 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | MINUS | MUL | NEQ | PARS | PLUS | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 162 "mmlparser.mly"
          ( Eq )
# 1218 "mmlparser.ml"
             in
            
# 121 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1223 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BOOL _ | CST _ | DIV | ELSE | EOF | IDENT _ | IN | LBRACE | MINUS | MUL | PARS | PLUS | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 160 "mmlparser.mly"
          ( Div )
# 1259 "mmlparser.ml"
             in
            
# 121 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1264 "mmlparser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | BOOL _ | CST _ | ELSE | EOF | IDENT _ | IN | LBRACE | MINUS | PARS | PLUS | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 159 "mmlparser.mly"
          ( Sub )
# 1304 "mmlparser.ml"
             in
            
# 121 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1309 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (f : (string * Mml.typ))), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 128 "mmlparser.mly"
                                ( Fun(fst(f),snd(f),e) )
# 1329 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState93 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | CST _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | FUN ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | IF ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LBRACKET ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState95 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | CST _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | FUN ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | IF ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LBRACKET ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MINUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (c : (Mml.expr))), _), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 124 "mmlparser.mly"
                              ( If(c,e,Unit) )
# 1498 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MINUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (c : (Mml.expr))), _), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 126 "mmlparser.mly"
                              ( If(c,e1,e2) )
# 1552 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState98 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | CST _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | FUN ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | IF ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LBRACKET ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99
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
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MINUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MINUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 1685 "mmlparser.ml"
            ))), _, (args : ((string * Mml.typ) list))), _, (t : (Mml.typ))), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 133 "mmlparser.mly"
                              ( let f = mk_fun args e1 in
                                let fix = Fix(id, t, f) in
                                Let(id, fix, e2) )
# 1692 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState106 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | CST _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | FUN ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | IF ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | LBRACKET ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MINUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MINUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | ELSE | EOF | IN | RPAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 1825 "mmlparser.ml"
            ))), _, (args : ((string * Mml.typ) list))), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 130 "mmlparser.mly"
                              ( let f = mk_fun args e1 in
                                Let(id, f , e2) )
# 1831 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | MINUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState109 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | CST _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | FUN ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | IF ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | LBRACKET ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | LET ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | IDENT _ | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (id : (
# 12 "mmlparser.mly"
       (string)
# 1911 "mmlparser.ml"
                ))), _, (e : (Mml.expr))), _) = _menhir_stack in
                let _v : (string * Mml.expr) = 
# 116 "mmlparser.mly"
    ( (id,e) )
# 1916 "mmlparser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
                | RBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (x : (string * Mml.expr))) = _menhir_stack in
                    let _v : ((string * Mml.expr) list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 1931 "mmlparser.ml"
                     in
                    _menhir_goto_nonempty_list_id_def_ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MINUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState116 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 110 "mmlparser.mly"
             ( e )
# 1994 "mmlparser.ml"
             in
            _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState119 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (types : ((string * Mml.strct) list))), _, (code : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.prog) = 
# 73 "mmlparser.mly"
    ( {types ; code} )
# 2024 "mmlparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Mml.prog)) = _v in
            Obj.magic _1
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | LE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | LT ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | MINUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | MOD ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | MUL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | NEQ ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | SEMI ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
    | _ ->
        _menhir_fail ()

and _menhir_reduce44 : _menhir_env -> (('ttv_tail * _menhir_state * (Mml.expr))) * (
# 12 "mmlparser.mly"
       (string)
# 2068 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (se : (Mml.expr))), (f : (
# 12 "mmlparser.mly"
       (string)
# 2074 "mmlparser.ml"
    ))) = _menhir_stack in
    let _v : (Mml.expr) = 
# 106 "mmlparser.mly"
             ( GetF(se, f) )
# 2079 "mmlparser.ml"
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
# 96 "mmlparser.mly"
             ( t )
# 2102 "mmlparser.ml"
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
# 94 "mmlparser.mly"
             ( TFun(t1, t2) )
# 2124 "mmlparser.ml"
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
# 2147 "mmlparser.ml"
            ))), _, (t : (Mml.typ))) = _menhir_stack in
            let _v : (string * Mml.typ * bool) = 
# 85 "mmlparser.mly"
    ( if mut!=None then (id,t,true) else (id,t,false) )
# 2152 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | MUTABLE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | IDENT _ ->
                _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Mml.typ * bool))) = _menhir_stack in
                let _v : (Mml.strct) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 2169 "mmlparser.ml"
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
# 2196 "mmlparser.ml"
            ))), _, (t : (Mml.typ))) = _menhir_stack in
            let _v : (string * Mml.typ) = 
# 149 "mmlparser.mly"
    ( (id,t) )
# 2201 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState55 ->
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
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
                    | CST _v ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
                    | FUN ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | IDENT _v ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
                    | IF ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | LBRACKET ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57
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
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState103 | MenhirState101 | MenhirState37 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAR ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | ASS | COLON ->
                    _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
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
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | CST _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | FUN ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | IF ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LBRACKET ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
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

and _menhir_reduce26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Mml.typ) list) = 
# 211 "<standard.mly>"
    ( [] )
# 2324 "mmlparser.ml"
     in
    _menhir_goto_list_fun_arg_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Mml.expr list) = 
# 211 "<standard.mly>"
    ( [] )
# 2333 "mmlparser.ml"
     in
    _menhir_goto_list_list_elem_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "mmlparser.mly"
       (int)
# 2340 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 10 "mmlparser.mly"
       (int)
# 2348 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 144 "mmlparser.mly"
          ( Int(n) )
# 2353 "mmlparser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CST _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | RBRACKET ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "mmlparser.mly"
       (string)
# 2372 "mmlparser.ml"
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
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | FUN ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | IF ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LBRACKET ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState34
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
    | MenhirState27 | MenhirState30 | MenhirState110 | MenhirState34 | MenhirState107 | MenhirState105 | MenhirState99 | MenhirState46 | MenhirState96 | MenhirState94 | MenhirState53 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState65 | MenhirState63 | MenhirState57 ->
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
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
                    | CST _v ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
                    | FUN ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                    | IDENT _v ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
                    | IF ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                    | LBRACKET ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
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
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
                | AND | BOOL _ | CST _ | DIV | DOT | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | SEMI | THEN ->
                    _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack)
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
# 120 "mmlparser.mly"
                              ( se )
# 2536 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 | MenhirState116 | MenhirState109 | MenhirState106 | MenhirState108 | MenhirState98 | MenhirState100 | MenhirState93 | MenhirState95 | MenhirState97 | MenhirState92 | MenhirState64 | MenhirState91 | MenhirState71 | MenhirState89 | MenhirState85 | MenhirState87 | MenhirState77 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState73 | MenhirState75 | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Mml.expr))), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 123 "mmlparser.mly"
                              ( App(e, se) )
# 2558 "mmlparser.ml"
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
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 153 "mmlparser.mly"
        ( Neg )
# 2580 "mmlparser.ml"
             in
            
# 122 "mmlparser.mly"
                              ( Uop(op, se) )
# 2585 "mmlparser.ml"
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
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RPAR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 152 "mmlparser.mly"
        ( Not )
# 2607 "mmlparser.ml"
             in
            
# 122 "mmlparser.mly"
                              ( Uop(op, se) )
# 2612 "mmlparser.ml"
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
# 91 "mmlparser.mly"
             ( TUnit )
# 2631 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.typ) = 
# 89 "mmlparser.mly"
             ( TInt )
# 2642 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.typ) = 
# 90 "mmlparser.mly"
             ( TBool )
# 2653 "mmlparser.ml"
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
# 2681 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (
# 12 "mmlparser.mly"
       (string)
# 2689 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.typ) = 
# 92 "mmlparser.mly"
             ( TStrct(id) )
# 2694 "mmlparser.ml"
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
# 103 "mmlparser.mly"
             ( Unit )
# 2710 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
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
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
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
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState30
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
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | ASS ->
            _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
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
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState37
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

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CST _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | RBRACKET ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

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

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | CST _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | FUN ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IF ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LBRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LET ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "mmlparser.mly"
       (string)
# 2913 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 12 "mmlparser.mly"
       (string)
# 2921 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 104 "mmlparser.mly"
             ( Var(x) )
# 2926 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "mmlparser.mly"
       (int)
# 2946 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 10 "mmlparser.mly"
       (int)
# 2954 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 101 "mmlparser.mly"
             ( Int(n) )
# 2959 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "mmlparser.mly"
       (bool)
# 2966 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 11 "mmlparser.mly"
       (bool)
# 2974 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 102 "mmlparser.mly"
             ( Bool(b) )
# 2979 "mmlparser.ml"
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
# 3039 "mmlparser.ml"
         in
        _menhir_goto_list_type_def_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | CST _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | FUN ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | IF ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LBRACKET ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState27
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

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) = 
# 114 "<standard.mly>"
    ( None )
# 3083 "mmlparser.ml"
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
# 3095 "mmlparser.ml"
     in
    _menhir_goto_option_MUTABLE_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
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
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Mml.strct) list) = 
# 211 "<standard.mly>"
    ( [] )
# 3379 "mmlparser.ml"
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
                    _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState4
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
    | BOOL _ | CST _ | FUN | IDENT _ | IF | LBRACE | LBRACKET | LET | LPAR | NEG | NOT | PARS ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3468 "mmlparser.ml"
