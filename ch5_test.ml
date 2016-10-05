open Ch5

let () = 
  (* reverse tests *)
  assert ((reverse []) = []);
  assert ((reverse [1]) = [1]);
  assert ((reverse [1;2]) = [2;1]);
  (* take tests *)
  assert ((take [] 0) = []);
  assert ((take [] 1) = []);
  assert ((take [1;2] 0) = []);
  assert ((take [1;2] 1) = [1]);
  assert ((take [1;2] 2) = [1;2]);
  assert ((take [1;2] 3) = [1;2]);
  (* drop tests *)
  assert ((drop [] 0) = []);
  assert ((drop [] 1) = []);
  assert ((drop [1;2] 0) = [1;2]);
  assert ((drop [1;2] 1) = [2]);
  assert ((drop [1;2] 2) = []);
  assert ((drop [1;2] 3) = []);

  (* merge_sort tests *)
  let op = (<=) in
  let list_len_op l1 l2 = (List.length l1) <= (List.length l2) in
  assert ((merge_sort [] op) = []);
  assert ((merge_sort [1] op) = [1]);
  assert ((merge_sort [1;2] op) = [1;2]);
  assert ((merge_sort [2;1] op) = [1;2]);
  assert ((merge_sort [1;2;3] op) = [1;2;3]);
  assert ((merge_sort [3;2;1] op) = [1;2;3]);
  assert ((merge_sort [3;1;2] op) = [1;2;3]); 
  assert ((merge_sort [[1;2;3];[1;2];[1];[]] list_len_op) = [[];[1];[1;2];[1;2;3]]);

  (* minf tests *)
  let f = (fun (x:int) -> x) in
  assert ((minimum f 10)  == 0);
  print_endline "Success"




