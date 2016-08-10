type 'a tree = 
    Lf
  | Br of 'a * 'a tree * 'a tree

let rec size = function
    Lf          -> 0
  | Br(v,t1,t2) -> 1 + size t1 + size t2

let rec depth = function
    Lf          -> 0
  | Br(v,t1,t2) -> 1 + max (depth t1) (depth t2)

let rec reflect = function
    Lf -> Lf
  | Br(v,t1,t2) -> Br(v, reflect t2, reflect t1)

let rec preord t acc = match t with
    Lf -> acc
  | Br(v,t1,t2) -> v :: (preord t1 (preord t2 acc))

let rec inord t acc = match t with
    Lf -> acc
  | Br(v,t1,t2) -> inord t1 (v :: (inord t2 acc))

let rec postord t acc = match t with
    Lf -> acc
  | Br(v,t1,t2) -> postord t1 (postord t2 (v::acc))

let rec balpre = function
    [] -> Lf
  | x::xs -> 
    let k = (List.length xs) / 2
    in Br(x, balpre (Utils.take xs k), balpre (Utils.drop xs k))

let rec balin = function 
    [] -> Lf
  | xs ->
    let k = (List.length xs) / 2 in
    match (Utils.drop xs k) with
      []    -> Lf
    | y::ys -> Br(y, balin (Utils.take xs k), balin ys)

let rec balpos = function 
    [] -> Lf
  | xs ->
    let k = (List.length xs) / 2 in
    let ys = (Utils.drop xs k) in
    match ys with
      [] -> Lf
    | zs -> 
      let j = (List.length zs) - 1 in
      let label = (Utils.last zs) in
      Br(label, balpos (Utils.take xs k), balpos (Utils.take zs j))

