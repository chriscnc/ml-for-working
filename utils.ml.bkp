

let last lst = List.hd (List.rev lst)


let rec take lst n = match lst with
    [] -> []
  | x::xs -> 
    if n > 0 then x::(take xs (n-1))
    else []


let rec drop lst n = match lst with
    [] -> []
  | x::xs ->
    if n > 0 then (drop xs (n-1))
    else lst


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




