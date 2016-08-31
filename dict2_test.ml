
let it = Dict2.insert Dict2.empty "France" 33

let ctree1 = Dict2.insert it "Egypt" 20

let it = Dict2.insert ctree1 "Hungary" 36

let it = Dict2.insert it "Mexico" 52

let ctree2 = Dict2.insert it "Japan" 81

let l1 = Dict2.lookup ctree1 "France"

let l2 = Dict2.lookup ctree2 "Mexico"

(* throws exception
let l3 = Dict.lookup ctree1 "Mexico"
*)



let () = 
  print_endline "Dict2 test success"

