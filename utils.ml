module type IUtils = 
  sig
    (* return the last elem of a list *)
    val last : 'a list -> 'a

    (* return a list of the first n elem of a list *)
    val take : 'a list -> int -> 'a list

    (* drop the first n elems of a list and return the
       remaining elems as a list. *)
    val drop : 'a list -> int -> 'a list

    (* convert a string to a list of chars *)
    val explode : string -> char list

    (* convert a list of chars to a string *)
    val implode : char list -> string
  end

module Utils : IUtils = 
  struct

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
  end





