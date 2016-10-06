(* return the last elem of a list *)
val last : 'a list -> 'a

(* return a list of the first n elem of a list *)
val take : 'a list -> int -> 'a list

(* drop the first n elems of a list and return the
   remaining elems as a list. *)
val drop : 'a list -> int -> 'a list

(* convert a string to a list of chars *)
val explode : string -> char list

(* convert a list of chars to a string *)
val implode : char list -> string

(* reverse a list *)
val reverse : 'a list -> 'a list
