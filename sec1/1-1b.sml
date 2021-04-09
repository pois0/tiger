type key = string
datatype 'a tree = LEAF | TREE of 'a tree * (key * 'a) * 'a tree
datatype 'a optional = SOME of 'a | NONE

val empty = LEAF

fun insert(LEAF, key, value) = TREE(LEAF, (key, value), LEAF)
  | insert(TREE(l, (k, v), r), key, value) =
      if key < k
        then TREE(insert(l, key, value), (k, v), r)
      else if key > k
        then TREE(l, (k, v), insert(r, key, value))
      else TREE(l, (k, v), r)

fun lookup(LEAF, key) = NONE
  | lookup(TREE(l, (k, v), r), key) =
      if key = k
        then SOME(v)
      else if key < k
        then lookup(l, key)
      else lookup(r, key)
