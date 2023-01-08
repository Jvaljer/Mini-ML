
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
    | LEN
    | LE
    | LBRACKET
    | LBRACE
    | LARROW
    | IN
    | IF
    | IDENT of (
# 12 "mmlparser.mly"
       (string)
# 48 "mmlparser.ml"
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
# 59 "mmlparser.ml"
  )
    | COMMA
    | COLON
    | BOOL of (
# 11 "mmlparser.mly"
       (bool)
# 66 "mmlparser.ml"
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
  | MenhirState143
  | MenhirState140
  | MenhirState137
  | MenhirState134
  | MenhirState133
  | MenhirState131
  | MenhirState129
  | MenhirState128
  | MenhirState127
  | MenhirState125
  | MenhirState124
  | MenhirState123
  | MenhirState120
  | MenhirState116
  | MenhirState112
  | MenhirState109
  | MenhirState105
  | MenhirState104
  | MenhirState103
  | MenhirState102
  | MenhirState100
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
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState63
  | MenhirState62
  | MenhirState58
  | MenhirState56
  | MenhirState55
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


# 171 "mmlparser.ml"

let rec _menhir_goto_nonempty_list_matching_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (possibilities : (Mml.expr list)) = _v in
        let (((_menhir_stack, _menhir_s), _, (e : (Mml.expr))), _) = _menhir_stack in
        let _v : (Mml.expr) = 
# 168 "mmlparser.mly"
                              ( MatchPattern(e,possibilities) )
# 184 "mmlparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Mml.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 195 "mmlparser.ml"
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
# 134 "mmlparser.mly"
             ( Strct(id_list) )
# 218 "mmlparser.ml"
             in
            _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.expr))), _, (xs : ((string * Mml.expr) list))) = _menhir_stack in
        let _v : ((string * Mml.expr) list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 234 "mmlparser.ml"
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
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SEMI | THEN | WITH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 255 "mmlparser.ml"
         in
        _menhir_goto_nonempty_list_matching_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ANYTHING ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState125 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Mml.expr) = 
# 177 "mmlparser.mly"
                    ( Anything )
# 278 "mmlparser.ml"
         in
        _menhir_goto_matching _menhir_env _menhir_stack _menhir_s _v
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66
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
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState68
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

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70
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

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80
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

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
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

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
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

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82
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
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84
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
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState86
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

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState88
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

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState78
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
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
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
# 767 "mmlparser.ml"
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
# 782 "mmlparser.ml"
            ))), _, (t_def : (Mml.strct))) = _menhir_stack in
            let _v : (string * Mml.strct) = 
# 105 "mmlparser.mly"
    ( (t_name,t_def) )
# 787 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | BOOL _ | CST _ | FUN | IDENT _ | IF | LBRACE | LEN | LET | LPAR | MATCH | NEG | NOT | PARS ->
                _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState24
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
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.typ))), _, (xs : ((string * Mml.typ) list))) = _menhir_stack in
        let _v : ((string * Mml.typ) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 872 "mmlparser.ml"
         in
        _menhir_goto_list_fun_arg_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState100 ->
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
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState102
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
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Mml.expr))), _, (xs : (Mml.expr list))) = _menhir_stack in
        let _v : (Mml.expr list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 935 "mmlparser.ml"
         in
        _menhir_goto_list_list_elem_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState109 ->
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
# 950 "mmlparser.ml"
            ))), _, (elems : (Mml.expr list))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 166 "mmlparser.mly"
                              ( ArrayInt(id,elems) )
# 955 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState120 ->
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
# 976 "mmlparser.ml"
            ))), _, (elems : (Mml.expr list))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 170 "mmlparser.mly"
                              ( Array(id,t,elems) )
# 981 "mmlparser.ml"
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

and _menhir_reduce51 : _menhir_env -> (('ttv_tail * _menhir_state * (Mml.expr))) * (
# 12 "mmlparser.mly"
       (string)
# 996 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (se : (Mml.expr))), (f : (
# 12 "mmlparser.mly"
       (string)
# 1002 "mmlparser.ml"
    ))) = _menhir_stack in
    let _v : (Mml.expr) = 
# 132 "mmlparser.mly"
             ( GetF(se, f) )
# 1007 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (se : (Mml.expr))), (f : (
# 12 "mmlparser.mly"
       (string)
# 1021 "mmlparser.ml"
        ))), _, (e : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr) = 
# 163 "mmlparser.mly"
                              ( SetF(se, f, e) )
# 1026 "mmlparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (f : (string * Mml.typ))), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 154 "mmlparser.mly"
                                ( Fun(fst(f),snd(f),e) )
# 1076 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState134 | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr) = 
# 164 "mmlparser.mly"
                              ( Seq(e1, e2) )
# 1090 "mmlparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | ELSE | EOF | EQ | IN | LE | LT | MINUS | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 195 "mmlparser.mly"
          ( Add )
# 1128 "mmlparser.ml"
             in
            
# 147 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1133 "mmlparser.ml"
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
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MOD | MUL | NEQ | OR | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 205 "mmlparser.mly"
          ( Or )
# 1167 "mmlparser.ml"
             in
            
# 147 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1172 "mmlparser.ml"
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
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | AND | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MOD | MUL | NEQ | OR | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 204 "mmlparser.mly"
          ( And )
# 1204 "mmlparser.ml"
             in
            
# 147 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1209 "mmlparser.ml"
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MUL | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 196 "mmlparser.mly"
          ( Mul )
# 1247 "mmlparser.ml"
             in
            
# 147 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1252 "mmlparser.ml"
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MOD | MUL | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 199 "mmlparser.mly"
          ( Mod )
# 1288 "mmlparser.ml"
             in
            
# 147 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1293 "mmlparser.ml"
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MUL | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 198 "mmlparser.mly"
          ( Div )
# 1331 "mmlparser.ml"
             in
            
# 147 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1336 "mmlparser.ml"
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | ELSE | EOF | EQ | IN | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 201 "mmlparser.mly"
          ( Neq )
# 1386 "mmlparser.ml"
             in
            
# 147 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1391 "mmlparser.ml"
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | ELSE | EOF | EQ | IN | LE | LT | MINUS | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 197 "mmlparser.mly"
          ( Sub )
# 1433 "mmlparser.ml"
             in
            
# 147 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1438 "mmlparser.ml"
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | ELSE | EOF | EQ | IN | LE | LT | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 202 "mmlparser.mly"
          ( Lt )
# 1484 "mmlparser.ml"
             in
            
# 147 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1489 "mmlparser.ml"
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | ELSE | EOF | EQ | IN | LE | LT | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 203 "mmlparser.mly"
          ( Le )
# 1535 "mmlparser.ml"
             in
            
# 147 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1540 "mmlparser.ml"
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | ELSE | EOF | EQ | IN | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 200 "mmlparser.mly"
          ( Eq )
# 1590 "mmlparser.ml"
             in
            
# 147 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1595 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState90 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState91
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState92 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState93
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
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (c : (Mml.expr))), _), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 150 "mmlparser.mly"
                              ( If(c,e,Unit) )
# 1768 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (c : (Mml.expr))), _), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 152 "mmlparser.mly"
                              ( If(c,e1,e2) )
# 1822 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState95 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState96
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
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState95
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 1957 "mmlparser.ml"
            ))), _, (args : ((string * Mml.typ) list))), _, (t : (Mml.typ))), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 159 "mmlparser.mly"
                              ( let f = mk_fun args e1 in
                                let fix = Fix(id, t, f) in
                                Let(id, fix, e2) )
# 1964 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState103 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 2099 "mmlparser.ml"
            ))), _, (args : ((string * Mml.typ) list))), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 156 "mmlparser.mly"
                              ( let f = mk_fun args e1 in
                                Let(id, f , e2) )
# 2105 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState123 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SELECT ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState127 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e_i : (Mml.expr))), _), _, (e_i_consequence : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 176 "mmlparser.mly"
                    ( MatchPossibility(e_i,e_i_consequence) )
# 2299 "mmlparser.ml"
             in
            _menhir_goto_matching _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState133 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | IDENT _ | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (id : (
# 12 "mmlparser.mly"
       (string)
# 2381 "mmlparser.ml"
                ))), _, (e : (Mml.expr))), _) = _menhir_stack in
                let _v : (string * Mml.expr) = 
# 142 "mmlparser.mly"
    ( (id,e) )
# 2386 "mmlparser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
                | RBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (x : (string * Mml.expr))) = _menhir_stack in
                    let _v : ((string * Mml.expr) list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 2401 "mmlparser.ml"
                     in
                    _menhir_goto_nonempty_list_id_def_ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState140 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 136 "mmlparser.mly"
             ( e )
# 2464 "mmlparser.ml"
             in
            _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState143 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (types : ((string * Mml.strct) list))), _, (code : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.prog) = 
# 99 "mmlparser.mly"
    ( {types ; code} )
# 2494 "mmlparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Mml.prog)) = _v in
            Obj.magic _1
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MINUS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MOD ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MUL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | NEQ ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | OR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | PLUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | SEMI ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
    | _ ->
        _menhir_fail ()

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
# 122 "mmlparser.mly"
             ( t )
# 2571 "mmlparser.ml"
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
# 120 "mmlparser.mly"
             ( TFun(t1, t2) )
# 2593 "mmlparser.ml"
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
# 2616 "mmlparser.ml"
            ))), _, (t : (Mml.typ))) = _menhir_stack in
            let _v : (string * Mml.typ * bool) = 
# 111 "mmlparser.mly"
    ( if mut!=None then (id,t,true) else (id,t,false) )
# 2621 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | MUTABLE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | IDENT _ ->
                _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Mml.typ * bool))) = _menhir_stack in
                let _v : (Mml.strct) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 2638 "mmlparser.ml"
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
# 2665 "mmlparser.ml"
            ))), _, (t : (Mml.typ))) = _menhir_stack in
            let _v : (string * Mml.typ) = 
# 187 "mmlparser.mly"
    ( (id,t) )
# 2670 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState56 ->
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
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
                    | CST _v ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
                    | FUN ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | IDENT _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
                    | IF ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | LEN ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58
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
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState100 | MenhirState98 | MenhirState38 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAR ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | ASS | COLON ->
                    _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
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
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
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
    | MenhirState116 ->
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
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
                    | CST _v ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
                    | IDENT _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState120
                    | LPAR ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState120
                    | PARS ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState120
                    | RBRACKET ->
                        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState120
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
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

and _menhir_reduce29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Mml.typ) list) = 
# 211 "<standard.mly>"
    ( [] )
# 2856 "mmlparser.ml"
     in
    _menhir_goto_list_fun_arg_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Mml.expr list) = 
# 211 "<standard.mly>"
    ( [] )
# 2865 "mmlparser.ml"
     in
    _menhir_goto_list_list_elem_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "mmlparser.mly"
       (string)
# 2872 "mmlparser.ml"
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
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | FUN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | IF ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LEN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState34
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
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (l : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 209 "mmlparser.mly"
          ( Len )
# 2982 "mmlparser.ml"
             in
            
# 171 "mmlparser.mly"
                                ( ListUop(op,l) )
# 2987 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 | MenhirState30 | MenhirState134 | MenhirState34 | MenhirState128 | MenhirState125 | MenhirState35 | MenhirState104 | MenhirState102 | MenhirState96 | MenhirState47 | MenhirState93 | MenhirState91 | MenhirState55 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState62 | MenhirState58 ->
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
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                    | CST _v ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                    | FUN ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | IDENT _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                    | IF ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | LEN ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | LET ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | LPAR ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | MATCH ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | NEG ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | NOT ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | PARS ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
                | AND | BOOL _ | CST _ | DIV | DOT | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
                    _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack)
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
# 146 "mmlparser.mly"
                              ( se )
# 3067 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState143 | MenhirState140 | MenhirState133 | MenhirState123 | MenhirState127 | MenhirState129 | MenhirState103 | MenhirState105 | MenhirState95 | MenhirState97 | MenhirState90 | MenhirState92 | MenhirState94 | MenhirState65 | MenhirState89 | MenhirState81 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState69 | MenhirState79 | MenhirState75 | MenhirState77 | MenhirState71 | MenhirState73 | MenhirState67 | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Mml.expr))), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 149 "mmlparser.mly"
                              ( App(e, se) )
# 3089 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState120 | MenhirState112 | MenhirState109 ->
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
# 3111 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | RBRACKET ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 191 "mmlparser.mly"
        ( Neg )
# 3157 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Uop(op, se) )
# 3162 "mmlparser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 190 "mmlparser.mly"
        ( Not )
# 3184 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Uop(op, se) )
# 3189 "mmlparser.ml"
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
# 117 "mmlparser.mly"
             ( TUnit )
# 3208 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.typ) = 
# 115 "mmlparser.mly"
             ( TInt )
# 3219 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.typ) = 
# 116 "mmlparser.mly"
             ( TBool )
# 3230 "mmlparser.ml"
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
# 3258 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (
# 12 "mmlparser.mly"
       (string)
# 3266 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.typ) = 
# 118 "mmlparser.mly"
             ( TStrct(id) )
# 3271 "mmlparser.ml"
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
# 129 "mmlparser.mly"
             ( Unit )
# 3287 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
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
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
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
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState35
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
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState30
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | LPAR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | TBOOL ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | TUNIT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
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
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
                    | CST _v ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
                    | IDENT _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState109
                    | LPAR ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState109
                    | PARS ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState109
                    | RBRACKET ->
                        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState109
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | ASS ->
            _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
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
                _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState38
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

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

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

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "mmlparser.mly"
       (string)
# 3615 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 12 "mmlparser.mly"
       (string)
# 3623 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 130 "mmlparser.mly"
             ( Var(x) )
# 3628 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "mmlparser.mly"
       (int)
# 3648 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 10 "mmlparser.mly"
       (int)
# 3656 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 127 "mmlparser.mly"
             ( Int(n) )
# 3661 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "mmlparser.mly"
       (bool)
# 3668 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 11 "mmlparser.mly"
       (bool)
# 3676 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 128 "mmlparser.mly"
             ( Bool(b) )
# 3681 "mmlparser.ml"
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
# 3741 "mmlparser.ml"
         in
        _menhir_goto_list_type_def_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | FUN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | IF ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LEN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState27
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

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) = 
# 114 "<standard.mly>"
    ( None )
# 3787 "mmlparser.ml"
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
# 3799 "mmlparser.ml"
     in
    _menhir_goto_option_MUTABLE_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
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
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
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

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Mml.strct) list) = 
# 211 "<standard.mly>"
    ( [] )
# 4127 "mmlparser.ml"
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
                    _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState4
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
    | BOOL _ | CST _ | FUN | IDENT _ | IF | LBRACE | LEN | LET | LPAR | MATCH | NEG | NOT | PARS ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 4216 "mmlparser.ml"
