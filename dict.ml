
type key = string
type 'a t = (key * 'a) Tree.t

exception E of key

let empty = Tree.Lf

let rec lookup t b = 
  let module T = Tree in
  match t with
    T.Lf -> raise (E b)
  | T.Br((a,x),t1,t2) ->
    let c = String.compare a b in
    if c = 0 then x else 
    if c > 0 then lookup t1 b
    else lookup t2 b


let rec insert t b y = 
  let module T = Tree in
  match t with
    T.Lf -> T.Br((b,y),T.Lf,T.Lf)
  | T.Br((a,x),t1,t2) ->
    let c = String.compare a b in
    if c = 0 then raise (E b) else
    if c > 0 then T.Br((a,x), (insert t1 b y), t2) 
    else T.Br((a,x), t1, (insert t2 b y))


let rec update t b y = 
  let module T = Tree in
  match t with
    T.Lf -> T.Br((b,y),T.Lf,T.Lf)
  | T.Br((a,x),t1,t2) ->
    let c = String.compare a b in
    if c = 0 then T.Br((a,y), t1, t2) else
    if c > 0 then T.Br((a,x), (update t1 b y), t2) 
    else T.Br((a,x), t2, (update t2 b y))
