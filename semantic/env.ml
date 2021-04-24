open Types

type env_entry =
  | VarEntry of { ty: typ }
  | FunEntry of { formals: typ list; res: typ }

let builtin_types = [
  ("Int", INT);
  ("string", STRING)
]

let builtin_funcs = [
  ("print", [ STRING ], UNIT);
  ("flush", [], UNIT);
  ("getchar", [], STRING);
  ("ord", [ STRING ], INT);
  ("chr", [ INT ], STRING);
  ("size", [ STRING ], INT);
  ("substring", [ STRING; INT; INT ], STRING);
  ("concat", [ STRING; STRING ], STRING);
  ("not", [ INT ], INT);
  ("exit", [ INT ], UNIT);
]

type venv = env_entry

type typenv = typ

type ctx = typ 
