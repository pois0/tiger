open Syntax.Ast
open Syntax.Sourcepos
open Types
open Symbol

exception TypeMismatch of pos * typ * typ
exception UndefinedType of symbol
exception UndefinedVar of symbol
exception UndefinedField of symbol
exception NotRecord of var

let type_mismatch pos expected actual =
  raise (TypeMismatch (pos, expected, actual))

let undefined_type sym = raise (UndefinedType sym)
let undefined_var sym = raise (UndefinedVar sym)
let undefined_field sym = raise (UndefinedField sym)
let not_record var = raise (NotRecord var)

let rec var_type_of ctx var =
  match var with
  | SimpleVar name ->
      let id = to_symbol name in
      Option.value (lookup id ctx) (undefined_var id)
  | FieldVar (v, name) -> 
      let id = to_symbol name in (
      match var_type_of ctx v with
      | RECORD (li, _) ->
          snd (Option.value
            (List.find_opt (fun (fld, _) -> fld == id) li)
            (undefined_field id)
          )
      | bad -> not_record v
      )
  | SubscriptVar (v, ewp) ->

let rec actual_type_of tenv t = 
  match t with
  | NameType name -> (
      let sym = to_symbol name in
      match lookup sym tenv with
        | Some ty -> ty
        | None -> undefined_type sym
    )
  | RecordType flds -> RECORD (
      List.map (fun { fld_name; fld_type } -> (to_symbol fld_name, actual_type_of tenv (NameType fld_type))) flds,
      ref ()
    )
  | ArrayType ty -> ARRAY (actual_type_of tenv (NameType ty), ref())

let rec type_of tenv ctx { value = exp; pos } =
  match exp with
  | NilExp -> NIL
  | IntExp _ -> INT
  | StringExp _ -> STRING
  | VarExp var -> var_type_of ctx var
  | AssignExp { var; exp } -> (*todo*) UNIT
  | UnaryOpExp { rator = _; rand } ->
      let _ = assert_type tenv ctx rand INT in
      INT
  | BinOpExp { op = _; left; right } ->
      let _ = assert_type tenv ctx left INT in
      let _ = assert_type tenv ctx right INT in
      INT
  | CallExp { rator; rands } -> (*todo*) UNIT
  | RecordExp {fields; typ } -> (*todo*) UNIT
  | SeqExp li ->
      List.fold_left
        (fun _ ewp -> type_of tenv ctx ewp)
        UNIT
        li
  | IfExp { cond; texp; fexp } ->
      let _ = assert_type tenv ctx cond INT in
      let tt = type_of tenv ctx texp in
      (match fexp with
        | Some fe ->
            let ft = type_of tenv ctx fe in
            if tt == ft then tt else type_mismatch pos tt ft
        | None -> UNIT)
  | WhileExp { cond; body } ->
      let _ = assert_type tenv ctx cond INT in
      let _ = type_of tenv ctx body in
      UNIT
  | ForExp { var; from; til; body } ->
      let _ = assert_type tenv ctx from INT in
      let _ = assert_type tenv ctx til INT in
      let _ = type_of tenv (*todo*) ctx body in
      UNIT
  | BreakExp -> UNIT
  | ArrayExp { typ; size; init } -> (**) UNIT
  | LetExp { decs; body } -> (**) UNIT

and assert_type tenv ctx ewp expected =
  let actual = type_of tenv ctx ewp in
  if expected == actual then actual
  else type_mismatch ewp.pos expected actual
