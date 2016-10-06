open Sequence


let () = 
  let finite = fromList [25;10] in
  let double x = x + x in
  let even x = (x mod 2) = 0 in
  assert ((hd (tl (from 1))) = 2);
  assert ((hd (tl (fromList [1;2;3]))) = 2);
  (* functions not in signature, created for exercises, etc 
  assert ((take1 (from1 1) 3) = [1;2;3]);
  assert ((take2 (from2 1) 3) = [1;2;3]);
  assert ((take (squares (from 1)) 3) = [1;4;9]);
  assert ((take (add (from 1) (from 1)) 3) = [2;4;6]);
  assert ((take (repeatEach (from 1) 2) 4) = [1;1;2;2]);
  assert ((repeatEach (from 1) 0) = Nil);
  assert ((repeatEach Nil 4) = Nil);
  assert ((toList (repeatEach (fromList [1;2;3]) 2)) = [1;1;2;2;3;3]);
  assert ((take (addAdj (from 1)) 3) = [3;7;11]);
  assert ((toList (addAdj (fromList [1;2;3;4;5;6]))) = [3;7;11]);
  assert ((addAdj Nil) = Nil);
     *)
  assert ((take (append finite (from 1415)) 3) = [25;10;1415]);
  assert ((take (interleave (from 1) (from 1)) 4) = [1;1;2;2]);
  assert ((take (map double (from 1)) 4) = [2;4;6;8]);
  assert ((take (filter even (from 1)) 4) = [2;4;6;8]);
  assert ((take (iterates double 1) 4) = [1;2;4;8]);
  assert ((take (iterates (fun x -> x /. 2.0) 1.0) 4) = [1.0;0.5;0.25;0.125]);
  assert ((null (from 1)) = false);
  assert ((null Nil) = true);
  assert ((take (drop (from 1) 3) 3) = [4;5;6]);
  assert ((take (drop (from 1) 0) 3) = [1;2;3]);
  assert ((drop Nil 3) = Nil);
  assert ((toList (fromList [1;2;3])) = [1;2;3]);
  assert ((toList (fromList [])) = []);
  assert ((take_while (from 1) (fun x -> x < 5)) = [1;2;3;4]);
  assert ((take_while Nil (fun x -> x < 5)) = []);
  assert ((take (drop_while (from 1) (fun x -> x < 5)) 4) = [5;6;7;8]);
  assert ((drop_while Nil (fun x -> x < 5)) = Nil);


  print_endline "Success"
