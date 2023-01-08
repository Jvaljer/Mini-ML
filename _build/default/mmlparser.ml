
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
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
    | LE
    | LBRACKET
    | LBRACE
    | LARROW
    | IN
    | IF
    | IDENT of (
# 12 "mmlparser.mly"
       (string)
# 47 "mmlparser.ml"
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
# 58 "mmlparser.ml"
  )
    | COMMA
    | COLON
    | BOOL of (
# 11 "mmlparser.mly"
       (bool)
# 65 "mmlparser.ml"
  )
    | ASS
    | ARRAY
    | ANYTHING
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
  | MenhirState141
  | MenhirState138
  | MenhirState135
  | MenhirState132
  | MenhirState131
  | MenhirState129
  | MenhirState127
  | MenhirState126
  | MenhirState125
  | MenhirState123
  | MenhirState122
  | MenhirState121
  | MenhirState118
  | MenhirState114
  | MenhirState110
  | MenhirState107
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState98
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
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState59
  | MenhirState58
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState47
  | MenhirState45
  | MenhirState41
  | MenhirState38
  | MenhirState35
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


# 169 "mmlparser.ml"

let rec _menhir_goto_nonempty_list_matching_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (possibilities : (Mml.expr list)) = _v in
        let (((_menhir_stack, _menhir_s), _, (e : (Mml.expr))), _) = _menhir_stack in
        let _v : (Mml.expr) = 
# 169 "mmlparser.mly"
                              ( MatchPattern(e,possibilities) )
# 182 "mmlparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Mml.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 193 "mmlparser.ml"
         in
        _menhir_goto_nonempty_list_matching_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_id_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Mml.expr) list) -> 'ttv_return =
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
# 135 "mmlparser.mly"
             ( Strct(id_list) )
# 216 "mmlparser.ml"
             in
            _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.expr))), _, (xs : ((string * Mml.expr) list))) = _menhir_stack in
        let _v : ((string * Mml.expr) list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 232 "mmlparser.ml"
         in
        _menhir_goto_nonempty_list_id_def_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_matching : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SELECT ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SEMI | THEN | WITH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 253 "mmlparser.ml"
         in
        _menhir_goto_nonempty_list_matching_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129

and _menhir_run123 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ANYTHING ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState123 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Mml.expr) = 
# 177 "mmlparser.mly"
                    ( Anything )
# 276 "mmlparser.ml"
         in
        _menhir_goto_matching _menhir_env _menhir_stack _menhir_s _v
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState78
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

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72
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

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState74
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

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80
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
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState82
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

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState84
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

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState86
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

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState76
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

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70
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
# 739 "mmlparser.ml"
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
# 754 "mmlparser.ml"
            ))), _, (t_def : (Mml.strct))) = _menhir_stack in
            let _v : (string * Mml.strct) = 
# 106 "mmlparser.mly"
    ( (t_name,t_def) )
# 759 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | BOOL _ | CST _ | FUN | IDENT _ | IF | LBRACE | LET | LPAR | MATCH | NEG | NOT | PARS ->
                _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState24
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
    | MenhirState38 ->
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LPAR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TBOOL ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TUNIT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.typ))), _, (xs : ((string * Mml.typ) list))) = _menhir_stack in
        let _v : ((string * Mml.typ) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 844 "mmlparser.ml"
         in
        _menhir_goto_list_fun_arg_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState98 ->
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
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | CST _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | FUN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | IF ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
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
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Mml.expr))), _, (xs : (Mml.expr list))) = _menhir_stack in
        let _v : (Mml.expr list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 905 "mmlparser.ml"
         in
        _menhir_goto_list_list_elem_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 920 "mmlparser.ml"
            ))), _, (elems : (Mml.expr list))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 167 "mmlparser.mly"
                              ( ArrayInt(id,elems) )
# 925 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (t : (Mml.typ))), (id : (
# 12 "mmlparser.mly"
       (string)
# 946 "mmlparser.ml"
            ))), _, (elems : (Mml.expr list))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 171 "mmlparser.mly"
                              ( Array(id,t,elems) )
# 951 "mmlparser.ml"
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

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (se : (Mml.expr))), (f : (
# 12 "mmlparser.mly"
       (string)
# 990 "mmlparser.ml"
        ))), _, (e : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr) = 
# 164 "mmlparser.mly"
                              ( SetF(se, f, e) )
# 995 "mmlparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (f : (string * Mml.typ))), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 155 "mmlparser.mly"
                                ( Fun(fst(f),snd(f),e) )
# 1045 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState132 | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr) = 
# 165 "mmlparser.mly"
                              ( Seq(e1, e2) )
# 1059 "mmlparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | ELSE | EOF | EQ | IN | LE | LT | MINUS | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 195 "mmlparser.mly"
          ( Add )
# 1097 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1102 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MOD | MUL | NEQ | OR | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 205 "mmlparser.mly"
          ( Or )
# 1136 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1141 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | AND | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MOD | MUL | NEQ | OR | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 204 "mmlparser.mly"
          ( And )
# 1173 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1178 "mmlparser.ml"
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
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MUL | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 196 "mmlparser.mly"
          ( Mul )
# 1216 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1221 "mmlparser.ml"
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
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MOD | MUL | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 199 "mmlparser.mly"
          ( Mod )
# 1257 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1262 "mmlparser.ml"
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
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MUL | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 198 "mmlparser.mly"
          ( Div )
# 1300 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1305 "mmlparser.ml"
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
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | ELSE | EOF | EQ | IN | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 201 "mmlparser.mly"
          ( Neq )
# 1355 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1360 "mmlparser.ml"
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
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | ELSE | EOF | EQ | IN | LE | LT | MINUS | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 197 "mmlparser.mly"
          ( Sub )
# 1402 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1407 "mmlparser.ml"
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
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | ELSE | EOF | EQ | IN | LE | LT | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 202 "mmlparser.mly"
          ( Lt )
# 1453 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1458 "mmlparser.ml"
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
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | ELSE | EOF | EQ | IN | LE | LT | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 203 "mmlparser.mly"
          ( Le )
# 1504 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1509 "mmlparser.ml"
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
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | ELSE | EOF | EQ | IN | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 200 "mmlparser.mly"
          ( Eq )
# 1559 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1564 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState88 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | CST _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | FUN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | IF ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState90 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | CST _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | FUN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | IF ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (c : (Mml.expr))), _), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 151 "mmlparser.mly"
                              ( If(c,e,Unit) )
# 1733 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (c : (Mml.expr))), _), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 153 "mmlparser.mly"
                              ( If(c,e1,e2) )
# 1787 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState93 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | CST _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | FUN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | IF ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState94
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
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState93
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
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 1920 "mmlparser.ml"
            ))), _, (args : ((string * Mml.typ) list))), _, (t : (Mml.typ))), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 160 "mmlparser.mly"
                              ( let f = mk_fun args e1 in
                                let fix = Fix(id, t, f) in
                                Let(id, fix, e2) )
# 1927 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState101 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | CST _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | FUN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IF ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 2060 "mmlparser.ml"
            ))), _, (args : ((string * Mml.typ) list))), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 157 "mmlparser.mly"
                              ( let f = mk_fun args e1 in
                                Let(id, f , e2) )
# 2066 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState121 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SELECT ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState125 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | CST _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | FUN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | IF ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e_i : (Mml.expr))), _), _, (e_i_consequence : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 176 "mmlparser.mly"
                    ( MatchPossibility(e_i,e_i_consequence) )
# 2258 "mmlparser.ml"
             in
            _menhir_goto_matching _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState131 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | CST _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | FUN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | IF ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | IDENT _ | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (id : (
# 12 "mmlparser.mly"
       (string)
# 2338 "mmlparser.ml"
                ))), _, (e : (Mml.expr))), _) = _menhir_stack in
                let _v : (string * Mml.expr) = 
# 143 "mmlparser.mly"
    ( (id,e) )
# 2343 "mmlparser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
                | RBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (x : (string * Mml.expr))) = _menhir_stack in
                    let _v : ((string * Mml.expr) list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 2358 "mmlparser.ml"
                     in
                    _menhir_goto_nonempty_list_id_def_ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState138 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 137 "mmlparser.mly"
             ( e )
# 2421 "mmlparser.ml"
             in
            _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | DIV ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState141 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (types : ((string * Mml.strct) list))), _, (code : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.prog) = 
# 100 "mmlparser.mly"
    ( {types ; code} )
# 2451 "mmlparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Mml.prog)) = _v in
            Obj.magic _1
        | EQ ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | MOD ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | MUL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | SEMI ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | _ ->
        _menhir_fail ()

and _menhir_reduce50 : _menhir_env -> (('ttv_tail * _menhir_state * (Mml.expr))) * (
# 12 "mmlparser.mly"
       (string)
# 2495 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (se : (Mml.expr))), (f : (
# 12 "mmlparser.mly"
       (string)
# 2501 "mmlparser.ml"
    ))) = _menhir_stack in
    let _v : (Mml.expr) = 
# 133 "mmlparser.mly"
             ( GetF(se, f) )
# 2506 "mmlparser.ml"
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
# 123 "mmlparser.mly"
             ( t )
# 2529 "mmlparser.ml"
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
        | ASS | IDENT _ | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Mml.typ))), _, (t2 : (Mml.typ))) = _menhir_stack in
            let _v : (Mml.typ) = 
# 121 "mmlparser.mly"
             ( TFun(t1, t2) )
# 2551 "mmlparser.ml"
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
# 2574 "mmlparser.ml"
            ))), _, (t : (Mml.typ))) = _menhir_stack in
            let _v : (string * Mml.typ * bool) = 
# 112 "mmlparser.mly"
    ( if mut!=None then (id,t,true) else (id,t,false) )
# 2579 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | MUTABLE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | IDENT _ ->
                _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Mml.typ * bool))) = _menhir_stack in
                let _v : (Mml.strct) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 2596 "mmlparser.ml"
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
    | MenhirState41 ->
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
# 2623 "mmlparser.ml"
            ))), _, (t : (Mml.typ))) = _menhir_stack in
            let _v : (string * Mml.typ) = 
# 187 "mmlparser.mly"
    ( (id,t) )
# 2628 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState50 ->
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
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
                    | CST _v ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
                    | FUN ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                    | IDENT _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
                    | IF ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                    | LET ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                    | LPAR ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                    | MATCH ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                    | NEG ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                    | NOT ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                    | PARS ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState98 | MenhirState96 | MenhirState38 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAR ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | ASS | COLON ->
                    _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
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
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | CST _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | FUN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | IF ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
        | RARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
                | LBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BOOL _v ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                    | CST _v ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                    | IDENT _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                    | LPAR ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                    | PARS ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                    | RBRACKET ->
                        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
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
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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

and _menhir_reduce28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Mml.typ) list) = 
# 211 "<standard.mly>"
    ( [] )
# 2810 "mmlparser.ml"
     in
    _menhir_goto_list_fun_arg_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Mml.expr list) = 
# 211 "<standard.mly>"
    ( [] )
# 2819 "mmlparser.ml"
     in
    _menhir_goto_list_list_elem_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "mmlparser.mly"
       (string)
# 2826 "mmlparser.ml"
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | FUN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | IF ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LET ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | MATCH ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
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

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | LPAR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | TBOOL ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | TUNIT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
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
    | MenhirState27 | MenhirState30 | MenhirState132 | MenhirState34 | MenhirState126 | MenhirState123 | MenhirState35 | MenhirState102 | MenhirState100 | MenhirState94 | MenhirState47 | MenhirState91 | MenhirState89 | MenhirState48 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState58 | MenhirState52 ->
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
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
                    | CST _v ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
                    | FUN ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | IDENT _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
                    | IF ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | LET ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | LPAR ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | MATCH ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | NEG ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | NOT ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | PARS ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
                | AND | BOOL _ | CST _ | DIV | DOT | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
                    _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack)
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
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 147 "mmlparser.mly"
                              ( se )
# 2990 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState141 | MenhirState138 | MenhirState131 | MenhirState121 | MenhirState125 | MenhirState127 | MenhirState101 | MenhirState103 | MenhirState93 | MenhirState95 | MenhirState88 | MenhirState90 | MenhirState92 | MenhirState63 | MenhirState87 | MenhirState79 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState67 | MenhirState77 | MenhirState73 | MenhirState75 | MenhirState69 | MenhirState71 | MenhirState65 | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Mml.expr))), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 150 "mmlparser.mly"
                              ( App(e, se) )
# 3012 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 | MenhirState110 | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (n : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 181 "mmlparser.mly"
                   ( n )
# 3034 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | CST _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | RBRACKET ->
                _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
        | DOT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 191 "mmlparser.mly"
        ( Neg )
# 3080 "mmlparser.ml"
             in
            
# 149 "mmlparser.mly"
                              ( Uop(op, se) )
# 3085 "mmlparser.ml"
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
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 190 "mmlparser.mly"
        ( Not )
# 3107 "mmlparser.ml"
             in
            
# 149 "mmlparser.mly"
                              ( Uop(op, se) )
# 3112 "mmlparser.ml"
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
# 118 "mmlparser.mly"
             ( TUnit )
# 3131 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.typ) = 
# 116 "mmlparser.mly"
             ( TInt )
# 3142 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.typ) = 
# 117 "mmlparser.mly"
             ( TBool )
# 3153 "mmlparser.ml"
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
# 3181 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (
# 12 "mmlparser.mly"
       (string)
# 3189 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.typ) = 
# 119 "mmlparser.mly"
             ( TStrct(id) )
# 3194 "mmlparser.ml"
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
# 130 "mmlparser.mly"
             ( Unit )
# 3210 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
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
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
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

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState30
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

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARRAY ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | LPAR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | TBOOL ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | TUNIT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
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
                | LBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BOOL _v ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                    | CST _v ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                    | IDENT _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | LPAR ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | PARS ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | RBRACKET ->
                        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ASS ->
            _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | COLON ->
                _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
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

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | CST _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | FUN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IF ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "mmlparser.mly"
       (string)
# 3509 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 12 "mmlparser.mly"
       (string)
# 3517 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 131 "mmlparser.mly"
             ( Var(x) )
# 3522 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "mmlparser.mly"
       (int)
# 3542 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 10 "mmlparser.mly"
       (int)
# 3550 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 128 "mmlparser.mly"
             ( Int(n) )
# 3555 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "mmlparser.mly"
       (bool)
# 3562 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 11 "mmlparser.mly"
       (bool)
# 3570 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 129 "mmlparser.mly"
             ( Bool(b) )
# 3575 "mmlparser.ml"
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
# 3635 "mmlparser.ml"
         in
        _menhir_goto_list_type_def_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | CST _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | FUN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | IF ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LET ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | MATCH ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState27
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

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) = 
# 114 "<standard.mly>"
    ( None )
# 3679 "mmlparser.ml"
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
# 3691 "mmlparser.ml"
     in
    _menhir_goto_option_MUTABLE_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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

and _menhir_reduce32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Mml.strct) list) = 
# 211 "<standard.mly>"
    ( [] )
# 4015 "mmlparser.ml"
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
                    _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState4
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
    | BOOL _ | CST _ | FUN | IDENT _ | IF | LBRACE | LET | LPAR | MATCH | NEG | NOT | PARS ->
        _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 4104 "mmlparser.ml"
