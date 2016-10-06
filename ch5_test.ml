open Ch5


let () = 
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




