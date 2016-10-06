type 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

exception Empty

(* these are all analogous to their List counterparts only for
   infinite sequences *)
val hd : 'a seq -> 'a
val tl : 'a seq -> 'a seq
val cons : 'a -> 'a seq -> 'a seq
val take : 'a seq -> int -> 'a list
val map : ('a -> 'b) -> 'a seq -> 'b seq
val filter : ('a -> bool) -> 'a seq -> 'a seq
val null : 'a seq -> bool
val drop : 'a seq -> int -> 'a seq
val take_while : 'a seq -> ('a -> bool) -> 'a list
val drop_while : 'a seq -> ('a -> bool) -> 'a seq

(* constucts a finite sequence from a list *)
val fromList : 'a list -> 'a seq

(* constructs an infinite sequences of integers starting from some number *)
val from : int -> int seq

(* appends a finite sequence onto either another finite sequence or and 
   infinite one *)
val append : 'a seq -> 'a seq -> 'a seq

(* interleaves the elements of two sequences *)
val interleave : 'a seq -> 'a seq -> 'a seq

(* creates an inifite sequence by iterating a fuction. (i.e.
   (x, f(x), f(f(x)),...) *)
val iterates : ('a -> 'a) -> 'a -> 'a seq

(* creates a list from a finite sequence *)
val toList : 'a seq -> 'a list



