type key = string
type 'a t = (key * 'a) list

exception E of key

let empty = []

let rec lookup t b = 
  match t with
    [] -> raise (E b)
  | (a,x)::tl ->
    let c = String.compare a b in
    if c = 0 then x else 
    if c > 0 then raise (E b)
    else lookup tl b


let rec insert t b y = 
  match t with
    [] -> (b,y)::t
  | (a,x)::tl ->
    let c = String.compare a b in
    if c = 0 then raise (E b) else
    if c > 0 then (b,y)::t
    else (a,x)::(insert tl b y)


let rec update t b y = 
  match t with
    []-> (b,y)::t
  | (a,x)::tl ->
    let c = String.compare a b in
    if c = 0 then (a,y)::tl else
    if c > 0 then (a,x)::(update tl b y)
    else (b,y)::t
