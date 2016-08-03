type ('a,'b) sum = 
    In1 of 'a
  | In2 of 'b

let rec concat1 = function 
    [] -> ""
  | In1(s)::rest -> s ^ (concat1 rest)
  | In2(_)::rest ->     (concat1 rest)


