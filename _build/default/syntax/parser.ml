
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | VAR
    | TYPE
    | TO
    | THEN
    | STR of (
# 11 "syntax/parser.mly"
       (string)
# 16 "syntax/parser.ml"
  )
    | SEMICOLON
    | RPAREN
    | RBRACKET
    | RBRACE
    | PLUS
    | OR
    | OF
    | NIL
    | NEQ
    | MUL
    | MINUS
    | LT
    | LPAREN
    | LET
    | LEQ
    | LBRACKET
    | LBRACE
    | INT of (
# 9 "syntax/parser.mly"
       (int)
# 38 "syntax/parser.ml"
  )
    | IN
    | IF
    | ID of (
# 10 "syntax/parser.mly"
       (string)
# 45 "syntax/parser.ml"
  )
    | GT
    | GEQ
    | FUNCTION
    | FOR
    | EQ
    | EOF
    | END
    | ELSE
    | DOT
    | DO
    | DIV
    | COMMA
    | COLON
    | BREAK
    | ASSIGN
    | ARRAY
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState134
  | MenhirState130
  | MenhirState124
  | MenhirState122
  | MenhirState119
  | MenhirState113
  | MenhirState109
  | MenhirState106
  | MenhirState94
  | MenhirState89
  | MenhirState84
  | MenhirState81
  | MenhirState79
  | MenhirState74
  | MenhirState70
  | MenhirState68
  | MenhirState66
  | MenhirState63
  | MenhirState61
  | MenhirState55
  | MenhirState53
  | MenhirState50
  | MenhirState47
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState25
  | MenhirState22
  | MenhirState18
  | MenhirState15
  | MenhirState13
  | MenhirState11
  | MenhirState6
  | MenhirState5
  | MenhirState4
  | MenhirState1
  | MenhirState0

# 1 "syntax/parser.mly"
  
  open Ast
  open Sourcepos

  let to_pos (p: Lexing.position) : pos = { lnum = p.pos_lnum; bol = p.pos_bol }
  let wp (exp: 'a) (pos: Lexing.position) : 'a with_pos = { value = exp; pos = to_pos(pos) }

# 133 "syntax/parser.ml"

let rec _menhir_goto_nonempty_list_dec_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dec list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.dec))), _, (xs : (Ast.dec list))) = _menhir_stack in
        let _v : (Ast.dec list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 188 "syntax/parser.ml"
         in
        _menhir_goto_nonempty_list_dec_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_tyfield__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.field list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (xs : (Ast.field list))) = _menhir_stack in
            let _v : (Ast.typ) = let l = 
# 232 "<standard.mly>"
    ( xs )
# 211 "syntax/parser.ml"
             in
            
# 121 "syntax/parser.mly"
    ( RecordType l )
# 216 "syntax/parser.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | EQ ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BREAK ->
                            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | FOR ->
                            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | ID _v ->
                            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | IF ->
                            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | INT _v ->
                            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | LET ->
                            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | LPAREN ->
                            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | MINUS ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | NIL ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | STR _v ->
                            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | WHILE ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
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
                | BREAK ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FOR ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | ID _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IF ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | INT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LET ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MINUS ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NIL ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STR _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_tyfield_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.field list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.field list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.field))) = _menhir_stack in
        let _v : (Ast.field list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 347 "syntax/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_tyfield_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState106 | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.field list)) = _v in
        let _v : (Ast.field list) = 
# 144 "<standard.mly>"
    ( x )
# 357 "syntax/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_tyfield__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_fundec_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.fundec list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState124 | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (fs : (Ast.fundec list)) = _v in
        let _v : (Ast.dec) = 
# 97 "syntax/parser.mly"
    ( FunctionDec fs )
# 373 "syntax/parser.ml"
         in
        _menhir_goto_dec _menhir_env _menhir_stack _menhir_s _v
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.fundec list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.fundec))) = _menhir_stack in
        let _v : (Ast.fundec list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 384 "syntax/parser.ml"
         in
        _menhir_goto_nonempty_list_fundec_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_dec : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dec) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNCTION ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | TYPE ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | VAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | IN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Ast.dec))) = _menhir_stack in
        let _v : (Ast.dec list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 409 "syntax/parser.ml"
         in
        _menhir_goto_nonempty_list_dec_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> (Ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (Ast.typ)) = _v in
    let ((_menhir_stack, _menhir_s), (i : (
# 10 "syntax/parser.mly"
       (string)
# 425 "syntax/parser.ml"
    )), _startpos_i_) = _menhir_stack in
    let _v : (Ast.dec) = 
# 115 "syntax/parser.mly"
    ( TypeDec { name=i; typ=t } )
# 430 "syntax/parser.ml"
     in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (ts : (Ast.dec)) = _v in
    let _v : (Ast.dec) = 
# 99 "syntax/parser.mly"
    ( ts )
# 438 "syntax/parser.ml"
     in
    _menhir_goto_dec _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.field list) = 
# 142 "<standard.mly>"
    ( [] )
# 447 "syntax/parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_tyfield__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "syntax/parser.mly"
       (string)
# 454 "syntax/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (ty : (
# 10 "syntax/parser.mly"
       (string)
# 474 "syntax/parser.ml"
            )) = _v in
            let _startpos_ty_ = _startpos in
            let (_menhir_stack, _menhir_s, (name : (
# 10 "syntax/parser.mly"
       (string)
# 480 "syntax/parser.ml"
            )), _startpos_name_) = _menhir_stack in
            let _v : (Ast.field) = 
# 127 "syntax/parser.mly"
    ( { fld_name=name; fld_type=ty } )
# 485 "syntax/parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
            | RBRACE | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Ast.field))) = _menhir_stack in
                let _v : (Ast.field list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 509 "syntax/parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_tyfield_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.with_pos list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.exp Ast.with_pos list)) = _v in
        let _v : (Ast.exp Ast.with_pos list) = 
# 144 "<standard.mly>"
    ( x )
# 546 "syntax/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.exp Ast.with_pos list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.exp Ast.with_pos)), _startpos_x_) = _menhir_stack in
        let _v : (Ast.exp Ast.with_pos list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 557 "syntax/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_fundec : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.fundec) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNCTION ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | IN | TYPE | VAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Ast.fundec))) = _menhir_stack in
        let _v : (Ast.fundec list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 578 "syntax/parser.ml"
         in
        _menhir_goto_nonempty_list_fundec_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122

and _menhir_goto_vardec : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dec) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (d : (Ast.dec)) = _v in
    let _v : (Ast.dec) = 
# 95 "syntax/parser.mly"
    ( d )
# 594 "syntax/parser.ml"
     in
    _menhir_goto_dec _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_rec_elm_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.record_elem list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.record_elem list)) = _v in
        let _v : (Ast.record_elem list) = 
# 144 "<standard.mly>"
    ( x )
# 608 "syntax/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_rec_elm__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.record_elem list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.record_elem))) = _menhir_stack in
        let _v : (Ast.record_elem list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 619 "syntax/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_rec_elm_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.with_pos list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.exp Ast.with_pos list)) = _v in
        let _v : (Ast.exp Ast.with_pos list) = 
# 144 "<standard.mly>"
    ( x )
# 635 "syntax/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.exp Ast.with_pos list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.exp Ast.with_pos)), _startpos_x_) = _menhir_stack in
        let _v : (Ast.exp Ast.with_pos list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 646 "syntax/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp Ast.with_pos) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.with_pos list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, (xs : (Ast.exp Ast.with_pos list))) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : (Ast.exp Ast.with_pos) = let exps = 
# 232 "<standard.mly>"
    ( xs )
# 1053 "syntax/parser.ml"
         in
        let _startpos = _startpos__1_ in
        
# 65 "syntax/parser.mly"
    ( wp (SeqExp exps) _startpos )
# 1059 "syntax/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ASSIGN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BREAK ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | FOR ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | ID _v ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | IF ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | INT _v ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LET ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LPAREN ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | MINUS ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | NIL ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | STR _v ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _, _), _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARRAY ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | OF ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (i : (
# 10 "syntax/parser.mly"
       (string)
# 1217 "syntax/parser.ml"
                        )) = _v in
                        let _startpos_i_ = _startpos in
                        let _v : (Ast.typ) = 
# 123 "syntax/parser.mly"
    ( ArrayType i )
# 1223 "syntax/parser.ml"
                         in
                        _menhir_goto_ty _menhir_env _menhir_stack _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (i : (
# 10 "syntax/parser.mly"
       (string)
# 1244 "syntax/parser.ml"
                )) = _v in
                let _startpos_i_ = _startpos in
                let _v : (Ast.typ) = 
# 119 "syntax/parser.mly"
    ( NameType i )
# 1250 "syntax/parser.ml"
                 in
                _menhir_goto_ty _menhir_env _menhir_stack _v
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RBRACE ->
                    _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run104 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_var : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BREAK ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NIL ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | DOT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (i : (
# 10 "syntax/parser.mly"
       (string)
# 1377 "syntax/parser.ml"
            )) = _v in
            let _startpos_i_ = _startpos in
            let (_menhir_stack, _menhir_s, (v : (Ast.var)), _startpos_v_) = _menhir_stack in
            let _startpos = _startpos_v_ in
            let _v : (Ast.var) = 
# 89 "syntax/parser.mly"
    ( FieldVar (v, i) )
# 1385 "syntax/parser.ml"
             in
            _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v _startpos
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | AND | COMMA | DIV | DO | ELSE | END | EOF | EQ | FUNCTION | GEQ | GT | IN | LEQ | LT | MINUS | MUL | NEQ | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (v : (Ast.var)), _startpos_v_) = _menhir_stack in
        let _startpos = _startpos_v_ in
        let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos_v_ in
        
# 53 "syntax/parser.mly"
    ( wp (VarExp v) _startpos )
# 1433 "syntax/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.with_pos list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (i : (
# 10 "syntax/parser.mly"
       (string)
# 1457 "syntax/parser.ml"
        )), _startpos_i_), _startpos__2_), _, (xs : (Ast.exp Ast.with_pos list))) = _menhir_stack in
        let _startpos = _startpos_i_ in
        let _v : (Ast.exp Ast.with_pos) = let args = 
# 232 "<standard.mly>"
    ( xs )
# 1463 "syntax/parser.ml"
         in
        let _startpos = _startpos_i_ in
        
# 61 "syntax/parser.mly"
    ( wp (CallExp { rator=i; rands=args }) _startpos )
# 1469 "syntax/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_rec_elm__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.record_elem list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i : (
# 10 "syntax/parser.mly"
       (string)
# 1493 "syntax/parser.ml"
        )), _startpos_i_), _, (xs : (Ast.record_elem list))) = _menhir_stack in
        let _startpos = _startpos_i_ in
        let _v : (Ast.exp Ast.with_pos) = let fields = 
# 232 "<standard.mly>"
    ( xs )
# 1499 "syntax/parser.ml"
         in
        let _startpos = _startpos_i_ in
        
# 63 "syntax/parser.mly"
    ( wp (RecordExp { fields=fields; typ=i }) _startpos )
# 1505 "syntax/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "syntax/parser.mly"
       (string)
# 1518 "syntax/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BREAK ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NIL ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.with_pos) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (v : (Ast.var)), _startpos_v_), _, (e : (Ast.exp Ast.with_pos)), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_v_ in
            let _v : (Ast.var) = 
# 91 "syntax/parser.mly"
    ( SubscriptVar (v, e) )
# 1605 "syntax/parser.ml"
             in
            _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | END | EOF | EQ | FUNCTION | GEQ | GT | IN | LEQ | LT | MINUS | NEQ | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (Ast.exp Ast.with_pos) = let o = 
# 130 "syntax/parser.mly"
          ( PlusOp )
# 1630 "syntax/parser.ml"
             in
            let _startpos = _startpos_l_ in
            
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 1636 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
        let _startpos = _startpos_l_ in
        let _v : (Ast.exp Ast.with_pos) = let o = 
# 132 "syntax/parser.mly"
          ( MulOp )
# 1653 "syntax/parser.ml"
         in
        let _startpos = _startpos_l_ in
        
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 1659 "syntax/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
        let _startpos = _startpos_l_ in
        let _v : (Ast.exp Ast.with_pos) = let o = 
# 133 "syntax/parser.mly"
          ( DivOp )
# 1670 "syntax/parser.ml"
         in
        let _startpos = _startpos_l_ in
        
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 1676 "syntax/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | OR | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (Ast.exp Ast.with_pos) = let o = 
# 141 "syntax/parser.mly"
          ( OrOp )
# 1713 "syntax/parser.ml"
             in
            let _startpos = _startpos_l_ in
            
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 1719 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | OR | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (Ast.exp Ast.with_pos) = let o = 
# 135 "syntax/parser.mly"
          ( NeqOp )
# 1748 "syntax/parser.ml"
             in
            let _startpos = _startpos_l_ in
            
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 1754 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | END | EOF | EQ | FUNCTION | GEQ | GT | IN | LEQ | LT | MINUS | NEQ | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _startpos__1_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (Ast.exp Ast.with_pos) = let o = 
# 131 "syntax/parser.mly"
          ( MinusOp )
# 1779 "syntax/parser.ml"
             in
            let _startpos = _startpos_l_ in
            
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 1785 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | OR | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (Ast.exp Ast.with_pos) = let o = 
# 136 "syntax/parser.mly"
          ( LtOp )
# 1814 "syntax/parser.ml"
             in
            let _startpos = _startpos_l_ in
            
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 1820 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | OR | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (Ast.exp Ast.with_pos) = let o = 
# 138 "syntax/parser.mly"
          ( LeqOp )
# 1849 "syntax/parser.ml"
             in
            let _startpos = _startpos_l_ in
            
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 1855 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | OR | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (Ast.exp Ast.with_pos) = let o = 
# 137 "syntax/parser.mly"
          ( GtOp )
# 1884 "syntax/parser.ml"
             in
            let _startpos = _startpos_l_ in
            
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 1890 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | OR | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (Ast.exp Ast.with_pos) = let o = 
# 139 "syntax/parser.mly"
          ( GeqOp )
# 1919 "syntax/parser.ml"
             in
            let _startpos = _startpos_l_ in
            
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 1925 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | OR | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (Ast.exp Ast.with_pos) = let o = 
# 134 "syntax/parser.mly"
          ( EqOp )
# 1954 "syntax/parser.ml"
             in
            let _startpos = _startpos_l_ in
            
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 1960 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | OR | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (Ast.exp Ast.with_pos)), _startpos_l_), _, (r : (Ast.exp Ast.with_pos)), _startpos_r_) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (Ast.exp Ast.with_pos) = let o = 
# 140 "syntax/parser.mly"
          ( AndOp )
# 2001 "syntax/parser.ml"
             in
            let _startpos = _startpos_l_ in
            
# 59 "syntax/parser.mly"
    ( wp (BinOpExp { op=o; left=l; right=r }) _startpos )
# 2007 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lval : (Ast.var)), _startpos_lval_), _, (e : (Ast.exp Ast.with_pos)), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_lval_ in
            let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos_lval_ in
            
# 55 "syntax/parser.mly"
    ( wp (AssignExp { var=lval; exp=e }) _startpos )
# 2053 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), (i : (
# 10 "syntax/parser.mly"
       (string)
# 2228 "syntax/parser.ml"
            )), _startpos_i_), _, (f : (Ast.exp Ast.with_pos)), _startpos_f_), _, (t : (Ast.exp Ast.with_pos)), _startpos_t_), _, (body : (Ast.exp Ast.with_pos)), _startpos_body_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos__1_ in
            
# 73 "syntax/parser.mly"
    ( wp (ForExp { var=i; from=f; til=t; body=body }) _startpos )
# 2235 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.exp Ast.with_pos)), _startpos_x_) = _menhir_stack in
            let _v : (Ast.exp Ast.with_pos list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2310 "syntax/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | OF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BREAK ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FOR ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | ID _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IF ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | INT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LET ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MINUS ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NIL ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STR _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (i : (
# 10 "syntax/parser.mly"
       (string)
# 2430 "syntax/parser.ml"
            )), _startpos_i_), _, (size : (Ast.exp Ast.with_pos)), _startpos_size_), _, (init : (Ast.exp Ast.with_pos)), _startpos_init_) = _menhir_stack in
            let _startpos = _startpos_i_ in
            let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos_i_ in
            
# 77 "syntax/parser.mly"
    ( wp (ArrayExp { typ=i; size=size; init=init }) _startpos )
# 2437 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (k : (
# 10 "syntax/parser.mly"
       (string)
# 2480 "syntax/parser.ml"
            )), _startpos_k_), _, (v : (Ast.exp Ast.with_pos)), _startpos_v_) = _menhir_stack in
            let _v : (Ast.record_elem) = 
# 83 "syntax/parser.mly"
    ( { key=k; exp=v } )
# 2485 "syntax/parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Ast.record_elem))) = _menhir_stack in
                let _v : (Ast.record_elem list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2509 "syntax/parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_rec_elm_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | END | EOF | FUNCTION | IN | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, (cond : (Ast.exp Ast.with_pos)), _startpos_cond_), _, (t : (Ast.exp Ast.with_pos)), _startpos_t_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos__1_ in
            
# 67 "syntax/parser.mly"
    ( wp (IfExp { cond=cond; texp=t; fexp=None }) _startpos )
# 2658 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (cond : (Ast.exp Ast.with_pos)), _startpos_cond_), _, (t : (Ast.exp Ast.with_pos)), _startpos_t_), _, (f : (Ast.exp Ast.with_pos)), _startpos_f_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos__1_ in
            
# 69 "syntax/parser.mly"
    ( wp (IfExp { cond=cond; texp=t; fexp=Some f }) _startpos )
# 2704 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | FUNCTION | IN | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (i : (
# 10 "syntax/parser.mly"
       (string)
# 2747 "syntax/parser.ml"
            )), _startpos_i_), (ty : (
# 10 "syntax/parser.mly"
       (string)
# 2751 "syntax/parser.ml"
            )), _startpos_ty_), _, (e : (Ast.exp Ast.with_pos)), _startpos_e_) = _menhir_stack in
            let _v : (Ast.dec) = 
# 103 "syntax/parser.mly"
    ( VarDec { name=i; typ=Some ty; init=e } )
# 2756 "syntax/parser.ml"
             in
            _menhir_goto_vardec _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | FUNCTION | IN | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (i : (
# 10 "syntax/parser.mly"
       (string)
# 2799 "syntax/parser.ml"
            )), _startpos_i_), _, (e : (Ast.exp Ast.with_pos)), _startpos_e_) = _menhir_stack in
            let _v : (Ast.dec) = 
# 105 "syntax/parser.mly"
    ( VarDec { name=i; typ=None; init=e } )
# 2804 "syntax/parser.ml"
             in
            _menhir_goto_vardec _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | FUNCTION | IN | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (i : (
# 10 "syntax/parser.mly"
       (string)
# 2847 "syntax/parser.ml"
            )), _startpos_i_), _startpos__3_), _, (xs : (Ast.field list))), _, (body : (Ast.exp Ast.with_pos)), _startpos_body_) = _menhir_stack in
            let _v : (Ast.fundec) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 2852 "syntax/parser.ml"
             in
            
# 111 "syntax/parser.mly"
    ( { fun_name=i; params=params; res_type=None; body=body } )
# 2857 "syntax/parser.ml"
             in
            _menhir_goto_fundec _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | FUNCTION | IN | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (i : (
# 10 "syntax/parser.mly"
       (string)
# 2900 "syntax/parser.ml"
            )), _startpos_i_), _startpos__3_), _, (xs : (Ast.field list))), (ty : (
# 10 "syntax/parser.mly"
       (string)
# 2904 "syntax/parser.ml"
            )), _startpos_ty_), _, (body : (Ast.exp Ast.with_pos)), _startpos_body_) = _menhir_stack in
            let _v : (Ast.fundec) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 2909 "syntax/parser.ml"
             in
            
# 109 "syntax/parser.mly"
    ( { fun_name=i; params=params; res_type=Some ty; body=body } )
# 2914 "syntax/parser.ml"
             in
            _menhir_goto_fundec _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, (decs : (Ast.dec list))), _, (body : (Ast.exp Ast.with_pos)), _startpos_body_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos__1_ in
            
# 79 "syntax/parser.mly"
    ( wp (LetExp { decs=decs; body=body }) _startpos )
# 2942 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState130 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.exp Ast.with_pos)), _startpos_x_) = _menhir_stack in
            let _v : (Ast.exp Ast.with_pos list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3037 "syntax/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (Ast.exp Ast.with_pos)), _startpos_e_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos__1_ in
        
# 57 "syntax/parser.mly"
    ( wp (UnaryOpExp { rator=NegOp; rand=e }) _startpos )
# 3055 "syntax/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | END | EOF | FUNCTION | IN | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, (cond : (Ast.exp Ast.with_pos)), _startpos_cond_), _, (body : (Ast.exp Ast.with_pos)), _startpos_body_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos__1_ in
            
# 71 "syntax/parser.mly"
    ( wp (WhileExp { cond=cond; body=body }) _startpos )
# 3161 "syntax/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.exp Ast.with_pos)), _startpos__1_) = _menhir_stack in
            let _v : (Ast.exp Ast.with_pos) = 
# 43 "syntax/parser.mly"
             ( _1 )
# 3186 "syntax/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.exp Ast.with_pos)) = _v in
            Obj.magic _1
        | EQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "syntax/parser.mly"
       (string)
# 3435 "syntax/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (
# 11 "syntax/parser.mly"
       (string)
# 3443 "syntax/parser.ml"
    )) = _v in
    let _startpos_s_ = _startpos in
    let _startpos = _startpos_s_ in
    let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos_s_ in
    
# 51 "syntax/parser.mly"
    ( wp (StringExp s) _startpos )
# 3451 "syntax/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos__1_ in
    
# 47 "syntax/parser.mly"
    ( wp NilExp _startpos )
# 3465 "syntax/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState5 in
        let _v : (Ast.exp Ast.with_pos list) = 
# 142 "<standard.mly>"
    ( [] )
# 3536 "syntax/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNCTION ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TYPE ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | VAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "syntax/parser.mly"
       (int)
# 3564 "syntax/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 9 "syntax/parser.mly"
       (int)
# 3572 "syntax/parser.ml"
    )) = _v in
    let _startpos_i_ = _startpos in
    let _startpos = _startpos_i_ in
    let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos_i_ in
    
# 49 "syntax/parser.mly"
    ( wp (IntExp i) _startpos )
# 3580 "syntax/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "syntax/parser.mly"
       (string)
# 3620 "syntax/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState68 in
            let _v : (Ast.record_elem list) = 
# 142 "<standard.mly>"
    ( [] )
# 3640 "syntax/parser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_rec_elm__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | LBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BREAK ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NIL ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BREAK ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NIL ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState15 in
            let _v : (Ast.exp Ast.with_pos list) = 
# 142 "<standard.mly>"
    ( [] )
# 3713 "syntax/parser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
    | AND | ASSIGN | COMMA | DIV | DO | DOT | ELSE | END | EOF | EQ | FUNCTION | GEQ | GT | IN | LEQ | LT | MINUS | MUL | NEQ | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN | TO | TYPE | VAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (
# 10 "syntax/parser.mly"
       (string)
# 3725 "syntax/parser.ml"
        )), _startpos_i_) = _menhir_stack in
        let _startpos = _startpos_i_ in
        let _v : (Ast.var) = 
# 87 "syntax/parser.mly"
    ( SimpleVar i )
# 3731 "syntax/parser.ml"
         in
        _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.exp Ast.with_pos) = let _startpos = _startpos__1_ in
    
# 75 "syntax/parser.mly"
    ( wp BreakExp _startpos )
# 3808 "syntax/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos

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

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.exp Ast.with_pos) =
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
    | BREAK ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3866 "syntax/parser.ml"
