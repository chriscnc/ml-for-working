open Utils

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
  print_endline "Success"


