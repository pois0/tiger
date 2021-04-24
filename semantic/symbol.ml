type symbol = (string * int)
[@@deriving show, eq]

let nextsym = ref 0
let init_size = 128

let hashtable = Hashtbl.create init_size

let name (s, _) = s

let to_symbol (name: string) =
  try
    let i = Hashtbl.find hashtable name in
    (name, i)
  with Not_found ->
    let i = !nextsym in
    nextsym := i + 1;
    Hashtbl.add hashtable name i;
    (name, i)

module Table = Map.Make (struct
  type t = symbol
  let compare (_, n1) (_, n2) = compare n1 n2
end)

type 'a table = 'a Table.t

let emtpy = Table.empty

let enter = Table.add

let lookup = Table.find_opt

let enter_list li table =
  List.fold_left
    (fun t (key, v) -> enter key v t)
    table
    li
