type 'a tree = 
    Lf
  | Br of 'a * 'a tree * 'a tree

(* size of the tree *)
val size : 'a tree -> int

(* depth of the tree *)
val depth: 'a tree -> int

(* reflect *)
val reflect: 'a tree -> 'a tree


