
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TYPE
  | TO
  | THEN
  | STR of (string)
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
  | INT of (int)
  | IN
  | IF
  | ID of (string)
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

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.exp Ast.with_pos)
