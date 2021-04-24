open Symbol

type unique = unit ref
[@@deriving show, eq]

type typ =
  | INT
  | STRING
  | RECORD of (symbol * typ) list * unique
  | ARRAY of typ * unique
  | NIL
  | UNIT
  | NAME of symbol * typ option ref
[@@deriving show, eq]
