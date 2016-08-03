
type 'a tree = 
    Lf
  | Br of 'a * 'a tree * 'a tree

let birnam = 
  Br("The", 
     Br("wood", 
        Lf,
        Br("of", 
           Br("Birnam", 
              Lf, 
              Lf), 
           Lf)),
     Lf)

let tree2 =
  Br(2, Br(1, Lf, Lf), Br(3, Lf, Lf))

let tree5 =
  Br(5, Br(6, Lf, Lf), Br(7, Lf, Lf))

let tree4 =
  Br(4, tree2, tree5)

let rec size = function
    Lf          -> 0
  | Br(v,t1,t2) -> 1 + size t1 + size t2

let rec depth = function
    Lf          -> 0
  | Br(v,t1,t2) -> 1 + max (depth t1) (depth t2)

let rec comptree (k,n) = 
  if n = 0 then Lf
           else Br(k, comptree(2*k, n-1),
                      comptree(2*k+1, n-1))

let rec reflect = function
    Lf -> Lf
  | Br(v,t1,t2) -> Br(v, reflect t2, reflect t1)

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

let test_obvious_bal () = 
  let b1 = Lf and
  b2 = Br(42,Lf,Lf) and
  b3 = Br(42, Br(42,Lf,Lf), Br(42,Lf,Lf)) and
  b4 = Br(42, Br(42,Lf,Lf), Lf) and
  b5 = Br(42, Br(42,Br(42,Lf,Lf), Br(42,Lf,Lf)), Lf) and
  b6 = Br(42, Br(42,Br(42,Br(42,Lf,Lf),Lf),Lf)
            , Br(42,Br(42,Br(42,Lf,Lf),Lf),Lf))
  in assert (obvious_bal b1);
  assert (obvious_bal b2);
  assert (obvious_bal b3);
  assert (obvious_bal b4);
  assert (not (obvious_bal b5));
  assert (not (obvious_bal b6));
  true;;

test_obvious_bal ();;

let test_balanced () = 
  let b1 = Lf and
  b2 = Br(42,Lf,Lf) and
  b3 = Br(42, Br(42,Lf,Lf), Br(42,Lf,Lf)) and
  b4 = Br(42, Br(42,Lf,Lf), Lf) and
  b5 = Br(42, Br(42,Br(42,Lf,Lf), Br(42,Lf,Lf)), Lf) and
  b6 = Br(42, Br(42,Br(42,Br(42,Lf,Lf),Lf),Lf)
            , Br(42,Br(42,Br(42,Lf,Lf),Lf),Lf))
  in assert (balanced b1);
  assert (balanced b2);
  assert (balanced b3);
  assert (balanced b4);
  assert (not (balanced b5));
(*  assert (not (balanced b6)); *)
  true;;

test_balanced ();;


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

let test_reflected () = 
  let ref_tree2 = reflect tree2 
  in assert (reflected ref_tree2 tree2);
  assert (not (reflected tree2 tree2));
  true;;

test_reflected ();;


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


let test_tree_enums () =
  assert ((preorder birnam) = ["The"; "wood"; "of"; "Birnam"]);
  assert ((preorder tree4) = [4;2;1;3;5;6;7]);
  assert ((inorder birnam) = ["wood"; "Birnam"; "of"; "The"]);
  assert ((inorder tree4) = [1;2;3;4;6;5;7]);
  assert ((postorder birnam) = ["Birnam";"of";"wood";"The"]);
  assert ((postorder tree4) = [1;3;2;6;7;5;4]);
  true;;

test_tree_enums ();;


let rec preord t acc = match t with
    Lf -> acc
  | Br(v,t1,t2) -> v :: (preord t1 (preord t2 acc))


let rec inord t acc = match t with
    Lf -> acc
  | Br(v,t1,t2) -> inord t1 (v :: (inord t2 acc))


let rec postord t acc = match t with
    Lf -> acc
  | Br(v,t1,t2) -> postord t1 (postord t2 (v::acc))


let test_tree_enums_optimized () =
  assert ((preord birnam []) = ["The"; "wood"; "of"; "Birnam"]);
  assert ((preord tree4 []) = [4;2;1;3;5;6;7]);
  assert ((inord birnam []) = ["wood"; "Birnam"; "of"; "The"]);
  assert ((inord tree4 []) = [1;2;3;4;6;5;7]);
  assert ((postord birnam []) = ["Birnam";"of";"wood";"The"]);
  assert ((postord tree4 []) = [1;3;2;6;7;5;4]);
  true;;

test_tree_enums_optimized ();;


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


(* Exercise 4.21
   Write a function to convert a postorder list of labels to a balanced tree. *)

let last lst = List.hd (List.rev lst)

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


let test_make_bal_tree () =
  assert (tree4 = (balpre [4;2;1;3;5;6;7]));
  assert (tree4 = (balin [1;2;3;4;6;5;7]));
  assert (tree4 = (balpos [1;3;2;6;7;5;4]));
  true;;

test_make_bal_tree ();;
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





