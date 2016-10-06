
let last lst = List.hd (List.rev lst)

let rec take ls k = match ls with
    [] -> []
  | x::xs -> 
    if k > 0 then x::(take xs (k - 1))
    else []


let rec drop ls k = match ls with
    [] -> []
  | x::xs ->
    if k > 0 then (drop xs (k - 1))
    else ls


let explode (s : string) = 
  let rec f i acc = 
    if i > 0 then f (i-1) (s.[i-1]::acc)
    else acc
  in
  f (String.length s) []


let implode (l : char list) =
  let res = Bytes.make (List.length l) ' ' in
  let rec f i l = match l with
      [] -> res
    | x::xs -> Bytes.set res i x; f (i + 1) xs
  in
  f 0 l


let reverse ls = 
  let rec reverse_acc ls acc = match ls with
      [] -> acc
    | x::xs -> (reverse_acc xs (x::acc))
  in
  reverse_acc ls []
