type 'a t = 
  | Lf 
  | Br of 'a * 'a t * 'a t

val size : 'a t -> int
val depth : 'a t -> int
val reflect : 'a t -> 'a t
val preord : 'a t -> 'a list -> 'a list
val inord : 'a t -> 'a list -> 'a list
val postord : 'a t -> 'a list -> 'a list
val balpre : 'a list -> 'a t
val balin : 'a list -> 'a t
val balpos : 'a list -> 'a t
