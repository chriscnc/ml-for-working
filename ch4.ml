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


type 'a tree = 
    Lf
  | Br of 'a * 'a tree * 'a tree

module Tree = 
struct
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
      in Br(x, balpre (take xs k), balpre (drop xs k))

  let rec balin = function 
      [] -> Lf
    | xs ->
      let k = (List.length xs) / 2 in
      match (drop xs k) with
        []    -> Lf
      | y::ys -> Br(y, balin (take xs k), balin ys)

  let rec balpos = function 
      [] -> Lf
    | xs ->
      let k = (List.length xs) / 2 in
      let ys = (drop xs k) in
      match ys with
        [] -> Lf
      | zs -> 
        let j = (List.length zs) - 1 in
        let label = (last zs) in
        Br(label, balpos (take xs k), balpos (take zs j))

end

module type Dictionary = 
sig
  type key
  type 'a t
  exception E of key
  val empty : 'a t
  val lookup : 'a t * key -> 'a
  val insert : 'a t * key * 'a -> 'a t
  val update : 'a t * key * 'a -> 'a t
end

module Dict : Dictionary = 
struct

  type key = string
  type 'a t = (key * 'a) tree

  exception E of key

  val empty = Lf

  let rec lookup = function
      (Lf, b) -> raise E b
    | (Br((a,x), t1, t2), b) ->
      match String.compare(a,b) with
        GREATER -> lookup(t1,b)
      | EQUAL -> x
      | LESS -> lookup(t2,b)

  let rec insert = function
      (Lf, b, y) -> Br((b,y), Lf, Lf)
    | (Br((a,x), t1, t2), b, y) ->
      match String.compare(a,b) with
        GREATER -> Br((a,x), insert (t1,b,y), t2)
      | EQUAL -> raise E b
      | LESS -> Br((a,x), t1, insert (t2,b,y))

  let rec update = function
      (Lf, b, y) -> Br((b,y), Lf, Lf)
    | (Br((a,x), t1, t2), b, y) ->
      match String.compar(a,b) with
        GREATER -> Br((a,x), update (t1,b,y), t2)
      | EQUAL -> Br((a,y), t1, t2)
      | LESS -> Br((a,x), t1, update (t2,b,y))

end


(*
let rec comptree (k,n) = 
  if n = 0 then Lf
           else Br(k, comptree(2*k, n-1),
                      comptree(2*k+1, n-1))


(* Exercise 4.13
   Write a function compsame(x,n) to construct a complete binary tree
   of depth n, labelling all nodes with x. How efficient is your function? *)
let rec compsame (x,n) = 
  if n = 0 then Lf
  else Br(x, compsame(x, n-1),
             compsame(x, n-1))

(* Exercise 4.14
   A binary tree is balanced (by size) if each node Br(x,t1,t2) satisfies
   |size(t1) - size(t2)| <= 1. The obvious recursive function to test whether
   a tree is balanced applies size at every subtree, performing much redundant
   computation. Write an efficient function to test whether a tree is balanced. *)


let rec obvious_bal = function
    Lf -> true
  | Br(v,t1,t2) -> (abs ((size t1) - (size t2))) <= 1 && 
                   (obvious_bal t1) && 
                   (obvious_bal t2)


let balanced t = 
  let rec bal = function
      Lf -> Some 0
    | Br(v,t1,t2) -> match ((bal t1),(bal t2)) with
        (Some(s1),Some(s2)) -> 
        if (abs (s1 - s2)) <= 1 
        then Some (1 + s1 + s2)
        else None
      | (Some(_),None) -> None
      | (None,Some(_)) -> None
      | (None,None) -> None
  in
  match bal t with 
    Some _ -> true
  | None -> false


let balanced = function
      Lf -> true
    | Br(v,t1,t2) -> let t1s = size t1 and
                         t2s = size t2
      in (abs (t1s - t2s)) <= 1 


(* Exercise 4.15
   Write a function that determines whether two arbitrary trees t and u satisfy 
   t = reflect(u). The function should not build any new trees, so it should not
   call reflect or Br, although it may use Br in patterns. *)

let rec reflected t u = match (t,u) with
    (Lf, Lf) -> true
  | (Br(v1,l1,r1), Br(v2,l2,r2)) ->
    v1 = v2 && (reflected l1 r2) && (reflected r1 l2)
  | (Br(_,_,_), Lf) -> false
  | (Lf, Br(_,_,_)) -> false


(* Exercise 4.16
   Lists need not have been built into ML. Give a datatype declaration of a type
   equivalent to 'a list. *)

type 'a mylist =
    Nil
  | Cons of 'a * 'a mylist

(* Exercise 4.17
   Declare a datatype ('a, 'b) ltree of labelled binary trees, where branch nodes 
   carry a label of type 'a and leaves carry a label of type 'b. *)

type ('a, 'b) ltree = 
    LLf of 'b
  | LBr of 'a * ('a, 'b) ltree * ('a, 'b) ltree

(* Exercise 4.18
   Declare a datatype of trees where each branch node may have any finite number 
   of branches. (Hint: use list.) *)

type 'a listtree = 
    ListLf 
  | ListBr of 'a * 'a listtree list 


let rec preorder = function
    Lf -> []
  | Br(v,t1,t2) -> [v] @ preorder t1 @ preorder t2


let rec inorder = function
    Lf -> []
  | Br(v,t1,t2) -> inorder t1 @ [v] @ inorder t2


let rec postorder = function
    Lf -> []
  | Br(v,t1,t2) -> postorder t1 @ postorder t2 @ [v]




(* Exercise 4.19
   Describe how inorder(birnam) and inord(birnam, []) are evaluated, reporting
   how many cons operations are performed 

   In inorder, for every branch of the tree there will be a number of conses equal
   to the number of nodes in the left branch, and one for the left branch. This given
   six for inorder.

   In inord, there will only be one cons per branch, for a total of 4.
  *)


(* Exercise 4.20
   Complete the following equations and explain why they are correct.

   preorder(reflect(t)) = rev(postorder(t))
   inorder(reflect(t)) = rev(inorder(t))
   postorder(reflect(t)) = rev(preorder(t))

   Not sure if this is what the question intends or not.
*)




(* Exercise 4.22
   The function balpre constructs one tree from a preorder list of labels. Write
   a function that, given a list of labels, constructs the list of all trees that
   have those labels in preorder *)

let all_balpre lst = 
  let rec range (n : int) (acc : int list) = 
    if n >= 0 then (range (n-1) (n::acc))
    else acc
  in 
  let rec balpre k = function
      [] -> Lf
    | x::xs -> Br(x, (balpre k xs), (balpre k xs))
  in
  List.map balpre (range ((List.length lst) - 1) [])



(* Exercise 4.23
   Let us put the datatype declaration inside the structure, then make the 
   constructors available outside using these declarations:

   val Lf = Tree.Lf;
   val Br = Tree.Br;

   What is wrong with this idea? *)

*)
