module type IDict = 
sig
  type key
  type 'a t
  exception E of key
  val empty : 'a t
  val lookup : 'a t * key -> 'a
  val insert : 'a t * key * 'a -> 'a t
  val update : 'a t * key * 'a -> 'a t
end

module Dict : IDict = 
struct

  type key = string
  type 'a t = (key * 'a) tree

  exception E of key

  let empty = Lf

  let rec lookup = function
      (Lf, b) -> raise (E b)
    | (Br((a,x), t1, t2), b) ->
      match String.compare (a,b) with
        GREATER -> lookup(t1,b)
      | EQUAL -> x
      | LESS -> lookup(t2,b)

  let rec insert = function
      (Lf, b, y) -> Br((b,y), Lf, Lf)
    | (Br((a,x), t1, t2), b, y) ->
      match String.compare (a,b) with
        GREATER -> Br((a,x), insert (t1,b,y), t2)
      | EQUAL -> raise (E b)
      | LESS -> Br((a,x), t1, insert (t2,b,y))

  let rec update = function
      (Lf, b, y) -> Br((b,y), Lf, Lf)
    | (Br((a,x), t1, t2), b, y) ->
      match String.compar (a,b) with
        GREATER -> Br((a,x), update (t1,b,y), t2)
      | EQUAL -> Br((a,y), t1, t2)
      | LESS -> Br((a,x), t1, update (t2,b,y))

end

