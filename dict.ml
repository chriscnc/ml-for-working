open Tree

let gt = 1
let eq = 0
let lt = -1

type key = string
type 'a t = (key * 'a) tree

exception E of key

let empty = Lf

let rec lookup tr ky = match tr with
    Lf -> raise (E ky)
  | Br((k,v),t1,t2) ->
    match String.compare k ky with
      gt -> lookup t1 ky
    | eq -> v
    | lt -> lookup t2 ky

let rec insert tr ky vl = match tr with
    Lf -> Br((ky,vl),Lf,Lf)
  | Br((k,v),t1,t2) ->
    match String.compare k ky with
      gt -> Br((k,v), (insert t1 ky vl), t2)
    | eq -> raise (E vl)
    | lt -> Br((k,v), t1, (insert t2 ky vl))

(* val update : 'a t -> key -> 'a -> 'a t *)
let rec update tr ky vl = match tr with
    Lf -> Br((ky,vl),Lf,Lf)
  | Br((k,v),t1,t2) ->
    match String.compare k ky with
      gt -> Br((k,v), (update t1 ky vl), t2)
    | eq -> Br((k,vl), t1, t2)
    | lt -> Br((k,v), t1, (update t2 ky vl))

