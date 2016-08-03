
let explode s = 
  let rec f i acc = 
    if i > 0 then f (i-1) (s.[i-1]::acc)
    else acc
  in
  f (String.length s) []


let implode l =
  let res = Bytes.make (List.length l) ' ' in
  let rec f i l = match l with
      [] -> res
    | x::xs -> Bytes.set res i x; f (i + 1) xs
  in
  f 0 l


  


