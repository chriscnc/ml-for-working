open Utils

let rec merge_sort ls less_eq_op = match ls with 
    []  -> []
  | [x] -> [x]
  | xs  ->
    let k = (List.length ls) / 2 in
    let rec merge l1 l2 = match (l1,l2) with
        (xs,[]) -> xs
      | ([],ys) -> ys
      | (x::xs,y::ys) ->
        if (less_eq_op x y) then x::(merge xs l2)
        else y::(merge l1 ys)
    in
    merge (merge_sort (take ls k) less_eq_op)
          (merge_sort (drop ls k) less_eq_op)


let minimum f m = 
  let rec minimum_acc m acc = 
    if m > 0 then acc
    else (minimum_acc (m-1) (min acc (f m)))
  in
  minimum_acc m (f 0)

