
type 'a seq = 
    Nil
  | Cons of 'a * (unit -> 'a seq)

exception Empty
exception Subscript

let rec hd s = match s with
    Cons(x,xf) -> x
  | Nil        -> raise Empty

let rec tl s = match s with
    Cons(x,xf) -> xf ()
  | Nil        -> raise Empty

let cons x xq = Cons(x, fun () -> xq)

let fromList l = List.fold_right cons l Nil

let rec from k = Cons(k, fun () -> from (k+1))

let rec take xq k = match (xq,k) with
    (_,0) -> []
  | (Nil,n) -> raise Subscript 
  | (Cons(x,xf),n) -> x::(take (xf ()) (n - 1))


let rec squares xq = match xq with
    Nil -> Nil
  | Cons(x,xf) -> Cons(x*x, (fun () -> squares (xf ())))

let rec add xq yq = match (xq,yq) with
    (Cons(x,xf),Cons(y,yf)) -> Cons(x + y, (fun () -> add (xf ()) (yf ())))
  | _ -> Nil

let rec append xq yq = match (xq,yq) with
    (Nil,yq) -> yq
  | (Cons(x,xf),yq) -> Cons(x, fun () -> append (xf ()) yq)

let rec interleave xq yq = match (xq,yq) with
    (Nil,yq) -> yq
  | (Cons(x,xf),yq) -> Cons(x, (fun () -> interleave yq (xf ())))

let rec map f xq = match xq with
    Nil -> Nil
  | Cons(x,xf) -> Cons((f x), (fun () -> map f (xf ())))

let rec filter p xq = match xq with
    Nil -> Nil
  | Cons(x,xf) -> 
    if (p x) then Cons(x, (fun () -> filter p (xf ())))
    else filter p (xf ())

let rec iterates f x = 
  Cons(x, fun () -> iterates f (f x))

let null xq = match xq with
    Nil -> true
  | Cons(_,_) -> false

let rec drop xq k = match xq with
    Nil -> Nil
  | Cons(x,xf) -> 
    if k > 0 then drop (xf ()) (k-1)
    else Cons(x, fun () -> (xf ()))

let rec toList xq = match xq with
    Nil -> []
  | Cons(x,xf) -> x::(toList (xf ()))

let rec take_while xq p = match xq with
    Nil -> []
  | Cons(x,xf) -> 
    if (p x) then x::(take_while (xf ()) p)
    else []

let rec drop_while xq p = match xq with
    Nil -> Nil
  | Cons(x,xf) ->
    if (p x) then drop_while (xf ()) p
    else Cons(x, fun () -> (xf ()))



(* exercise functions *)
let rec repeatEach xq n = match (xq,n) with
    (_,0) -> Nil
  | (Nil,_) -> Nil
  | (Cons(x,xf),n) -> repeatElem x n n (xf ())
and repeatElem x k n remainingSeq = 
  if k > 0 then Cons(x, fun () -> repeatElem x (k - 1) n remainingSeq)
  else repeatEach remainingSeq n 

let rec addAdj xq = match xq with
    Nil -> Nil
  | Cons(x,xf) -> 
    match (xf ()) with
      Nil -> Nil
    | Cons(y,yf) -> Cons(x+y, fun () -> addAdj (yf ()))

type 'a seq1 = 
    Nil1
  | Cons1 of (unit -> 'a * 'a seq1)

let rec from1 k = Cons1(fun () -> (k, (from1 (k+1))))

let rec take1 xq k = match (xq,k) with
    (_,0)        -> []
  | (Nil1,n)     -> raise Subscript
  | (Cons1(xf),n) -> 
    let (rx, rxq) = xf () 
    in rx::(take1 rxq (n-1))

type 'a seq2node = 
    Nil2
  | Cons2 of 'a * 'a seq2
and
  'a seq2 = Seq2 of (unit -> 'a seq2node)

let rec from2 k = Seq2(fun () -> Cons2(k, (from2 (k+1))))

let rec take2 xq k = match (xq,k) with
    (_,0) -> []
  | (Seq2(f),n) -> 
    match (f ()) with
      Nil2 -> []
    | Cons2(y,yq) -> y::(take2 yq (k-1))

