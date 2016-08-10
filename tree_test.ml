open Tree

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


let test_reflected () = 
  let ref_tree2 = reflect tree2 
  in assert (reflected ref_tree2 tree2);
  assert (not (reflected tree2 tree2));
  true;;

test_reflected ();;



let test_tree_enums () =
  assert ((preorder birnam) = ["The"; "wood"; "of"; "Birnam"]);
  assert ((preorder tree4) = [4;2;1;3;5;6;7]);
  assert ((inorder birnam) = ["wood"; "Birnam"; "of"; "The"]);
  assert ((inorder tree4) = [1;2;3;4;6;5;7]);
  assert ((postorder birnam) = ["Birnam";"of";"wood";"The"]);
  assert ((postorder tree4) = [1;3;2;6;7;5;4]);
  true;;

test_tree_enums ();;


let test_tree_enums_optimized () =
  assert ((preord birnam []) = ["The"; "wood"; "of"; "Birnam"]);
  assert ((preord tree4 []) = [4;2;1;3;5;6;7]);
  assert ((inord birnam []) = ["wood"; "Birnam"; "of"; "The"]);
  assert ((inord tree4 []) = [1;2;3;4;6;5;7]);
  assert ((postord birnam []) = ["Birnam";"of";"wood";"The"]);
  assert ((postord tree4 []) = [1;3;2;6;7;5;4]);
  true;;

test_tree_enums_optimized ();;



let test_make_bal_tree () =
  assert (tree4 = (balpre [4;2;1;3;5;6;7]));
  assert (tree4 = (balin [1;2;3;4;6;5;7]));
  assert (tree4 = (balpos [1;3;2;6;7;5;4]));
  true;;

test_make_bal_tree ();;

