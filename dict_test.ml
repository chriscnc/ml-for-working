
let it = Dict.insert Dict.empty "France" 33

let ctree1 = Dict.insert it "Egypt" 20

let it = Dict.insert ctree1 "Hungary" 36

let it = Dict.insert it "Mexico" 52

let ctree2 = Dict.insert it "Japan" 81

let l1 = Dict.lookup ctree1 "France"

let l2 = Dict.lookup ctree2 "Mexico"

(* throws exception
let l3 = Dict.lookup ctree1 "Mexico"
*)

let it = Tree.inord ctree2 []

let baltree = Tree.balin it


let () = 
  print_endline "Dict test success"

