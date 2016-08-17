open Tree

let birnam = Br("The", Br("wood", Lf, Br("of", Br("Birnam", Lf, Lf), Lf)), Lf)
let tree2 = Br(2, Br(1, Lf, Lf), Br(3, Lf, Lf))
let tree5 = Br(5, Br(6, Lf, Lf), Br(7, Lf, Lf))
let tree4 = Br(4, tree2, tree5)

let () = 
  assert (tree2 = (reflect (reflect tree2)));
  print_endline "Success"
