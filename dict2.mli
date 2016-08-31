type key = string
type 'a t = (key * 'a) list
exception E of key
val empty  : 'a t
val lookup : 'a t -> key -> 'a
val insert : 'a t -> key -> 'a -> 'a t 
val update : 'a t -> key -> 'a -> 'a t
