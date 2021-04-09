{
    open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = alpha+ (alpha | digit | '_')*
let num = digit+
let ws = ['\t' ' ' '\n']

rule token = parse
    | ws+           { token lexbuf }
    | eof           { EOF }
    | ','           { COMMA }
    | '.'           { DOT }
    | ':'           { COLON }
    | ';'           { SEMICOLON }
    | '('           { LPAREN }
    | ')'           { RPAREN }
    | '{'           { LBRACE }
    | '}'           { RBRACE }
    | '['           { LBRACKET }
    | ']'           { RBRACKET }
    | ":="          { ASSIGN }
    | '+'           { PLUS }
    | '-'           { MINUS }
    | '*'           { MUL }
    | '/'           { DIV }
    | '='           { EQ }
    | "<>"          { NEQ }
    | "<"           { LT }
    | ">"           { GT }
    | "<="          { LEQ }
    | ">="          { GEQ }
    | '&'           { AND }
    | '|'           { OR }
    | "type"        { TYPE }
    | "array"       { ARRAY }
    | "of"          { OF }
    | "var"         { VAR }
    | "nil"         { NIL }
    | "function"    { FUNCTION }
    | "let"         { LET }
    | "in"          { IN }
    | "end"         { END }
    | "if"          { IF }
    | "then"        { THEN }
    | "else"        { ELSE }
    | "while"       { WHILE }
    | "for"         { FOR }
    | "do"          { DO }
    | "to"          { TO }
    | "break"       { BREAK }
    | num as n      { INT(int_of_string n) }
    | id as i       { ID i }
    | '"' _* '"' as str { let s = String.sub str 1 ((String.length str) - 2) in STR s }
    | "/*" _* "/*"  { token lexbuf }
