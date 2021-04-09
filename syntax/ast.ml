open Sourcepos

type 'a with_pos = { value: 'a; pos: pos }
[@@deriving show, eq]

and exp =
  | NilExp
  | IntExp of int
  | StringExp of string
  | VarExp of var
  | AssignExp of { var: var; exp: exp with_pos }
  | UnaryOpExp of { rator: unaryop; rand: exp with_pos }
  | BinOpExp of { op: binop; left: exp with_pos; right: exp with_pos }
  | CallExp of { rator: string; rands: exp with_pos list }
  | RecordExp of { fields: record_elem list; typ: string }
  | SeqExp of exp with_pos list
  | IfExp of { cond: exp with_pos; texp: exp with_pos; fexp: exp with_pos option }
  | WhileExp of { cond: exp with_pos; body: exp with_pos }
  | ForExp of { var: string; from: exp with_pos; til: exp with_pos; body: exp with_pos }
  | BreakExp
  | ArrayExp of { typ: string; size: exp with_pos; init: exp with_pos }
  | LetExp of { decs: dec list; body: exp with_pos }
[@@deriving show, eq]

and var =
  | SimpleVar of string
  | FieldVar of var * string
  | SubscriptVar of var * (exp with_pos)
[@@deriving show, eq]

and record_elem = { key: string; exp: exp with_pos }
[@@deriving show, eq]

and dec =
  | VarDec of { name: string; typ: string option; init: exp with_pos }
  | FunctionDec of fundec list
  | TypeDec of { name: string; typ: typ }
[@@deriving show, eq]

and fundec = { fun_name: string; params: field list; res_type: string option; body: exp with_pos }

and typ =
  | NameType of string
  | RecordType of field list
  | ArrayType of string
[@@deriving show, eq]

and field = { fld_name: string; fld_type: string; }
[@@deriving show, eq]

and unaryop =
  | NegOp

and binop =
  | PlusOp
  | MinusOp
  | MulOp
  | DivOp
  | EqOp
  | NeqOp
  | LtOp
  | GtOp
  | LeqOp
  | GeqOp
  | AndOp
  | OrOp
[@@deriving show, eq]
