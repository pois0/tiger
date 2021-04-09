%{
  open Ast
  open Sourcepos

  let to_pos (p: Lexing.position) : pos = { lnum = p.pos_lnum; bol = p.pos_bol }
  let wp (exp: 'a) (pos: Lexing.position) : 'a with_pos = { value = exp; pos = to_pos(pos) }
%}

%token <int>    INT
%token <string> ID
%token <string> STR
%token EOF
%token COMMA
%token DOT
%token COLON
%token SEMICOLON
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token ASSIGN
%token PLUS MINUS MUL DIV
%token EQ NEQ LT GT LEQ GEQ
%token AND OR
%token TYPE ARRAY OF VAR NIL FUNCTION LET IN END IF THEN ELSE WHILE FOR DO TO BREAK


%nonassoc DO THEN OF
%nonassoc ELSE
%nonassoc ASSIGN

%left OR
%left AND
%nonassoc EQ NEQ LT GT GEQ LEQ
%left PLUS MINUS
%left MUL DIV
%nonassoc UMINUS

%start <exp with_pos> prog

%%

prog:
  | expr EOF { $1 }

expr:
  | NIL 
    { wp NilExp $startpos }
  | i=INT
    { wp (IntExp i) $startpos }
  | s=STR
    { wp (StringExp s) $startpos }
  | v=var
    { wp (VarExp v) $startpos }
  | lval=var ASSIGN e=expr
    { wp (AssignExp { var=lval; exp=e }) $startpos }
  | MINUS e = expr %prec UMINUS
    { wp (UnaryOpExp { rator=NegOp; rand=e }) $startpos }
  | l=expr o=binop r=expr
    { wp (BinOpExp { op=o; left=l; right=r }) $startpos }
  | i=ID LPAREN args=separated_list(COMMA, expr) RPAREN
    { wp (CallExp { rator=i; rands=args }) $startpos }
  | i=ID LBRACE fields=separated_list(COMMA, rec_elm) RBRACE
    { wp (RecordExp { fields=fields; typ=i }) $startpos }
  | LPAREN exps=separated_list(SEMICOLON, expr) RPAREN
    { wp (SeqExp exps) $startpos }
  | IF cond=expr THEN t=expr
    { wp (IfExp { cond=cond; texp=t; fexp=None }) $startpos }
  | IF cond=expr THEN t=expr ELSE f=expr
    { wp (IfExp { cond=cond; texp=t; fexp=Some f }) $startpos }
  | WHILE cond=expr DO body=expr
    { wp (WhileExp { cond=cond; body=body }) $startpos }
  | FOR i=ID ASSIGN f=expr TO t=expr DO body=expr
    { wp (ForExp { var=i; from=f; til=t; body=body }) $startpos }
  | BREAK
    { wp BreakExp $startpos }
  | i=ID LBRACKET size=expr RBRACKET OF init=expr
    { wp (ArrayExp { typ=i; size=size; init=init }) $startpos }
  | LET decs=nonempty_list(dec) IN body=expr END
    { wp (LetExp { decs=decs; body=body }) $startpos }

rec_elm:
  | k=ID EQ v=expr
    { { key=k; exp=v } }

var:
  | i=ID
    { SimpleVar i }
  | v=var DOT i=ID
    { FieldVar (v, i) }
  | v=var DOT LBRACKET e=expr RBRACKET
    { SubscriptVar (v, e) }

dec:
  | d=vardec
    { d }
  | fs=nonempty_list(fundec)
    { FunctionDec fs }
  | ts=tydec
    { ts }

vardec:
  | VAR i=ID COLON ty=ID ASSIGN e=expr
    { VarDec { name=i; typ=Some ty; init=e } }
  | VAR i=ID ASSIGN e=expr
    { VarDec { name=i; typ=None; init=e } }

fundec:
  | FUNCTION i=ID LPAREN params=separated_list(COMMA, tyfield) RPAREN COLON ty=ID EQ body=expr
    { { fun_name=i; params=params; res_type=Some ty; body=body } }
  | FUNCTION i=ID LPAREN params=separated_list(COMMA, tyfield) RPAREN EQ body=expr
    { { fun_name=i; params=params; res_type=None; body=body } }

tydec:
  | TYPE i=ID EQ t=ty
    { TypeDec { name=i; typ=t } }

ty:
  | i=ID
    { NameType i }
  | LBRACE l=separated_list(COMMA, tyfield) RBRACE
    { RecordType l }
  | ARRAY OF i=ID
    { ArrayType i }
  

tyfield:
  | name=ID COLON ty=ID
    { { fld_name=name; fld_type=ty } }

%inline binop:
  | PLUS  { PlusOp }
  | MINUS { MinusOp }
  | MUL   { MulOp }
  | DIV   { DivOp }
  | EQ    { EqOp }
  | NEQ   { NeqOp }
  | LT    { LtOp }
  | GT    { GtOp }
  | LEQ   { LeqOp }
  | GEQ   { GeqOp }
  | AND   { AndOp }
  | OR    { OrOp }
