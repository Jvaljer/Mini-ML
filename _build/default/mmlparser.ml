
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
    | CONCAT
    | COMMA
    | COLON
    | BOOL of (
# 11 "mmlparser.mly"
       (bool)
# 67 "mmlparser.ml"
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
  | MenhirState142
  | MenhirState139
  | MenhirState136
  | MenhirState133
  | MenhirState132
  | MenhirState130
  | MenhirState128
  | MenhirState127
  | MenhirState126
  | MenhirState124
  | MenhirState123
  | MenhirState122
  | MenhirState118
  | MenhirState115
  | MenhirState111
  | MenhirState109
  | MenhirState108
  | MenhirState107
  | MenhirState106
  | MenhirState104
  | MenhirState102
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
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState64
  | MenhirState60
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


# 173 "mmlparser.ml"

let rec _menhir_goto_nonempty_list_matching_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (possibilities : (Mml.expr list)) = _v in
        let (((_menhir_stack, _menhir_s), _, (e : (Mml.expr))), _) = _menhir_stack in
        let _v : (Mml.expr) = 
# 167 "mmlparser.mly"
                              ( MatchPattern(e,possibilities) )
# 186 "mmlparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Mml.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 197 "mmlparser.ml"
         in
        _menhir_goto_nonempty_list_matching_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_list_elem_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Mml.expr))), _, (xs : (Mml.expr list))) = _menhir_stack in
        let _v : (Mml.expr list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 214 "mmlparser.ml"
         in
        _menhir_goto_list_list_elem_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState115 ->
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
# 229 "mmlparser.ml"
            ))), _, (elems : (Mml.expr list))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 169 "mmlparser.mly"
                              ( Array(id,t,elems) )
# 234 "mmlparser.ml"
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
# 263 "mmlparser.ml"
             in
            _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.expr))), _, (xs : ((string * Mml.expr) list))) = _menhir_stack in
        let _v : ((string * Mml.expr) list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 279 "mmlparser.ml"
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
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SEMI | THEN | WITH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 300 "mmlparser.ml"
         in
        _menhir_goto_nonempty_list_matching_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130

and _menhir_run124 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ANYTHING ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState124 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Mml.expr) = 
# 178 "mmlparser.mly"
                    ( Anything )
# 323 "mmlparser.ml"
         in
        _menhir_goto_matching _menhir_env _menhir_stack _menhir_s _v
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

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

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
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

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * (Mml.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | CST _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | FUN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | IF ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | LBRACE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | LEN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | LET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | LPAR ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | MATCH ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NEG ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NOT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | PARS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

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
# 812 "mmlparser.ml"
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
# 827 "mmlparser.ml"
            ))), _, (t_def : (Mml.strct))) = _menhir_stack in
            let _v : (string * Mml.strct) = 
# 106 "mmlparser.mly"
    ( (t_name,t_def) )
# 832 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | BOOL _ | CST _ | FUN | IDENT _ | IF | LBRACE | LEN | LET | LPAR | MATCH | NEG | NOT | PARS ->
                _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState24
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
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Mml.typ))), _, (xs : ((string * Mml.typ) list))) = _menhir_stack in
        let _v : ((string * Mml.typ) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 917 "mmlparser.ml"
         in
        _menhir_goto_list_fun_arg_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState104 ->
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
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Mml.expr list) = 
# 211 "<standard.mly>"
    ( [] )
# 974 "mmlparser.ml"
     in
    _menhir_goto_list_list_elem_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce52 : _menhir_env -> (('ttv_tail * _menhir_state * (Mml.expr))) * (
# 12 "mmlparser.mly"
       (string)
# 981 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (se : (Mml.expr))), (f : (
# 12 "mmlparser.mly"
       (string)
# 987 "mmlparser.ml"
    ))) = _menhir_stack in
    let _v : (Mml.expr) = 
# 133 "mmlparser.mly"
             ( GetF(se, f) )
# 992 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Mml.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (se : (Mml.expr))), (f : (
# 12 "mmlparser.mly"
       (string)
# 1006 "mmlparser.ml"
        ))), _, (e : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr) = 
# 164 "mmlparser.mly"
                              ( SetF(se, f, e) )
# 1011 "mmlparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (f : (string * Mml.typ))), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 155 "mmlparser.mly"
                                ( Fun(fst(f),snd(f),e) )
# 1061 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState133 | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
        let _v : (Mml.expr) = 
# 165 "mmlparser.mly"
                              ( Seq(e1, e2) )
# 1075 "mmlparser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | ELSE | EOF | EQ | IN | LE | LT | MINUS | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 196 "mmlparser.mly"
          ( Add )
# 1113 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1118 "mmlparser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75
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
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MOD | MUL | NEQ | OR | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 206 "mmlparser.mly"
          ( Or )
# 1152 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1157 "mmlparser.ml"
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
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | AND | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MOD | MUL | NEQ | OR | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 205 "mmlparser.mly"
          ( And )
# 1189 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1194 "mmlparser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState79
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
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MUL | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 197 "mmlparser.mly"
          ( Mul )
# 1232 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1237 "mmlparser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MOD | MUL | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 200 "mmlparser.mly"
          ( Mod )
# 1273 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1278 "mmlparser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DIV | ELSE | EOF | EQ | IN | LE | LT | MINUS | MUL | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 199 "mmlparser.mly"
          ( Div )
# 1316 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1321 "mmlparser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | ELSE | EOF | EQ | IN | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 202 "mmlparser.mly"
          ( Neq )
# 1371 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1376 "mmlparser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | ELSE | EOF | EQ | IN | LE | LT | MINUS | NEQ | PLUS | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 198 "mmlparser.mly"
          ( Sub )
# 1418 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1423 "mmlparser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | ELSE | EOF | EQ | IN | LE | LT | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 203 "mmlparser.mly"
          ( Lt )
# 1469 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1474 "mmlparser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | ELSE | EOF | EQ | IN | LE | LT | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 204 "mmlparser.mly"
          ( Le )
# 1520 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1525 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | ELSE | EOF | EQ | IN | NEQ | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 201 "mmlparser.mly"
          ( Eq )
# 1575 "mmlparser.ml"
             in
            
# 148 "mmlparser.mly"
                              ( Bop(op, e1, e2) )
# 1580 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState94 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState96 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (c : (Mml.expr))), _), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 151 "mmlparser.mly"
                              ( If(c,e,Unit) )
# 1753 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (c : (Mml.expr))), _), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 153 "mmlparser.mly"
                              ( If(c,e1,e2) )
# 1807 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState99 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100
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
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 1942 "mmlparser.ml"
            ))), _, (args : ((string * Mml.typ) list))), _, (t : (Mml.typ))), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 160 "mmlparser.mly"
                              ( let f = mk_fun args e1 in
                                let fix = Fix(id, t, f) in
                                Let(id, fix, e2) )
# 1949 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState107 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (id : (
# 12 "mmlparser.mly"
       (string)
# 2084 "mmlparser.ml"
            ))), _, (args : ((string * Mml.typ) list))), _, (e1 : (Mml.expr))), _), _, (e2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 157 "mmlparser.mly"
                              ( let f = mk_fun args e1 in
                                Let(id, f , e2) )
# 2090 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState122 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SELECT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState126 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | ELSE | EOF | IN | RARROW | RPAR | SELECT | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e_i : (Mml.expr))), _), _, (e_i_consequence : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 177 "mmlparser.mly"
                    ( MatchPossibility(e_i,e_i_consequence) )
# 2284 "mmlparser.ml"
             in
            _menhir_goto_matching _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState132 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | FUN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | IF ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | LEN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | LET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | MATCH ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | NEG ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | NOT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | IDENT _ | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (id : (
# 12 "mmlparser.mly"
       (string)
# 2366 "mmlparser.ml"
                ))), _, (e : (Mml.expr))), _) = _menhir_stack in
                let _v : (string * Mml.expr) = 
# 143 "mmlparser.mly"
    ( (id,e) )
# 2371 "mmlparser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
                | RBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (x : (string * Mml.expr))) = _menhir_stack in
                    let _v : ((string * Mml.expr) list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 2386 "mmlparser.ml"
                     in
                    _menhir_goto_nonempty_list_id_def_ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState139 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 137 "mmlparser.mly"
             ( e )
# 2449 "mmlparser.ml"
             in
            _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | BOOL _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
        | CST _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
        | DIV ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState142 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (types : ((string * Mml.strct) list))), _, (code : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.prog) = 
# 100 "mmlparser.mly"
    ( {types ; code} )
# 2479 "mmlparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Mml.prog)) = _v in
            Obj.magic _1
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
        | LBRACE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | LPAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | MINUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | MOD ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | MUL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | OR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | PARS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | SEMI ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
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
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack)
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
# 123 "mmlparser.mly"
             ( t )
# 2556 "mmlparser.ml"
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
# 2578 "mmlparser.ml"
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
# 2601 "mmlparser.ml"
            ))), _, (t : (Mml.typ))) = _menhir_stack in
            let _v : (string * Mml.typ * bool) = 
# 112 "mmlparser.mly"
    ( if mut!=None then (id,t,true) else (id,t,false) )
# 2606 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | MUTABLE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | IDENT _ ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Mml.typ * bool))) = _menhir_stack in
                let _v : (Mml.strct) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 2623 "mmlparser.ml"
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
# 2650 "mmlparser.ml"
            ))), _, (t : (Mml.typ))) = _menhir_stack in
            let _v : (string * Mml.typ) = 
# 188 "mmlparser.mly"
    ( (id,t) )
# 2655 "mmlparser.ml"
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
            | MenhirState104 | MenhirState102 | MenhirState38 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAR ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | ASS | COLON ->
                    _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
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
    | MenhirState111 ->
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
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                    | CST _v ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                    | IDENT _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | LPAR ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | PARS ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | RBRACKET ->
                        _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
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

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Mml.typ) list) = 
# 211 "<standard.mly>"
    ( [] )
# 2841 "mmlparser.ml"
     in
    _menhir_goto_list_fun_arg_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "mmlparser.mly"
       (string)
# 2848 "mmlparser.ml"
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
# 210 "mmlparser.mly"
          ( Len )
# 2958 "mmlparser.ml"
             in
            
# 170 "mmlparser.mly"
                              ( ListUop(op,l) )
# 2963 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 | MenhirState30 | MenhirState133 | MenhirState34 | MenhirState127 | MenhirState124 | MenhirState35 | MenhirState108 | MenhirState106 | MenhirState100 | MenhirState47 | MenhirState97 | MenhirState95 | MenhirState55 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState64 | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCAT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
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
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
                    | CST _v ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
                    | FUN ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState64
                    | IDENT _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
                    | IF ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState64
                    | LBRACE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64
                    | LEN ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64
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
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
                | AND | BOOL _ | CONCAT | CST _ | DIV | DOT | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
                    _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack)
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
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (se : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = 
# 147 "mmlparser.mly"
                              ( se )
# 3085 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l1 : (Mml.expr))), _, (l2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 214 "mmlparser.mly"
       ( EqL )
# 3107 "mmlparser.ml"
             in
            
# 172 "mmlparser.mly"
                              ( ListBop(op,l1,l2) )
# 3112 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 | MenhirState139 | MenhirState132 | MenhirState122 | MenhirState126 | MenhirState128 | MenhirState107 | MenhirState109 | MenhirState99 | MenhirState101 | MenhirState94 | MenhirState96 | MenhirState98 | MenhirState69 | MenhirState93 | MenhirState85 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState73 | MenhirState83 | MenhirState79 | MenhirState81 | MenhirState75 | MenhirState77 | MenhirState71 | MenhirState65 ->
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
# 150 "mmlparser.mly"
                              ( App(e, se) )
# 3134 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | BOOL _ | CST _ | DIV | ELSE | EOF | EQ | IDENT _ | IN | LBRACE | LE | LPAR | LT | MINUS | MOD | MUL | NEQ | OR | PARS | PLUS | RARROW | RPAR | SELECT | SEMI | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l1 : (Mml.expr))), _, (l2 : (Mml.expr))) = _menhir_stack in
            let _v : (Mml.expr) = let op = 
# 213 "mmlparser.mly"
           ( Concat )
# 3156 "mmlparser.ml"
             in
            
# 172 "mmlparser.mly"
                              ( ListBop(op,l1,l2) )
# 3161 "mmlparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 | MenhirState115 ->
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
# 182 "mmlparser.mly"
                   ( n )
# 3183 "mmlparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | CST _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | LBRACE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | LPAR ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | PARS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | RBRACKET ->
                _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
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
# 192 "mmlparser.mly"
        ( Neg )
# 3229 "mmlparser.ml"
             in
            
# 149 "mmlparser.mly"
                              ( Uop(op, se) )
# 3234 "mmlparser.ml"
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
# 191 "mmlparser.mly"
        ( Not )
# 3256 "mmlparser.ml"
             in
            
# 149 "mmlparser.mly"
                              ( Uop(op, se) )
# 3261 "mmlparser.ml"
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
# 3280 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.typ) = 
# 116 "mmlparser.mly"
             ( TInt )
# 3291 "mmlparser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Mml.typ) = 
# 117 "mmlparser.mly"
             ( TBool )
# 3302 "mmlparser.ml"
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
# 3330 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (
# 12 "mmlparser.mly"
       (string)
# 3338 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.typ) = 
# 119 "mmlparser.mly"
             ( TStrct(id) )
# 3343 "mmlparser.ml"
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
# 3359 "mmlparser.ml"
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | LPAR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | TBOOL ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | TUNIT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | ASS ->
            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
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
                _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState38
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
# 3641 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 12 "mmlparser.mly"
       (string)
# 3649 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 131 "mmlparser.mly"
             ( Var(x) )
# 3654 "mmlparser.ml"
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
# 3674 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 10 "mmlparser.mly"
       (int)
# 3682 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 128 "mmlparser.mly"
             ( Int(n) )
# 3687 "mmlparser.ml"
     in
    _menhir_goto_s_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "mmlparser.mly"
       (bool)
# 3694 "mmlparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 11 "mmlparser.mly"
       (bool)
# 3702 "mmlparser.ml"
    )) = _v in
    let _v : (Mml.expr) = 
# 129 "mmlparser.mly"
             ( Bool(b) )
# 3707 "mmlparser.ml"
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
# 3767 "mmlparser.ml"
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

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) = 
# 114 "<standard.mly>"
    ( None )
# 3813 "mmlparser.ml"
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
# 3825 "mmlparser.ml"
     in
    _menhir_goto_option_MUTABLE_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Mml.strct) list) = 
# 211 "<standard.mly>"
    ( [] )
# 4157 "mmlparser.ml"
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
                    _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState4
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
        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 4246 "mmlparser.ml"
