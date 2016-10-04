
type prop = 
    Atom of string
  | Neg of prop
  | Conj of prop * prop
  | Disj of prop * prop

let implies p q = Disj(Neg p, q)

let rec show p = match p with
    Atom a -> a
  | Neg p  -> "(~" ^ show p ^ ")"
  | Conj(p,q) -> "(" ^ show p ^ " & " ^ show q ^ ")"
  | Disj(p,q) -> "(" ^ show p ^ " | " ^ show q ^ ")"

let showp p =
  let rec f k p = match p with
      Atom a -> a
    | Neg p -> "~" ^ (f 3 p)
    | Conj(p,q) ->
      if k > 2 then "(" ^ (f k p) ^ " & " ^ (f k q) ^ ")"
      else (f 2 p) ^ " & " ^ (f 2 q)
    | Disj(p,q) -> 
      if k > 1 then "(" ^ (f k p) ^ " | " ^ (f k q) ^ ")"
      else (f 1 p) ^ " | " ^ (f 1 q)
  in f 0 p


let rec eval p ts = match p with
    Atom a    -> List.mem a ts
  | Neg p     -> not (eval p ts)
  | Conj(p,q) -> (eval p ts) && (eval q ts)
  | Disj(p,q) -> (eval p ts) || (eval q ts)


let rec nnf prop = match prop with
    Atom a        -> Atom a
  | Neg (Atom a)  -> Neg (Atom a)
  | Neg (Neg p)   -> nnf p
  | Neg Conj(p,q) -> (Disj(nnf (Neg p), nnf (Neg q)))
  | Neg Disj(p,q) -> (Conj(nnf (Neg p), nnf (Neg q)))
  | Conj(p,q)     -> Conj(nnf p, nnf q)
  | Disj(p,q)     -> Disj(nnf p, nnf q)


let rec distrib pp pq = match (pp,pq) with
    (p, Conj(q,r)) -> Conj((distrib p q), distrib p r)
  | (Conj(q,r), p) -> Conj(distrib q p, distrib r p)
  | (p, q)         -> Disj(p,q) (* no conjunctions *)


let rec cnf prop = match prop with
    Conj(p,q) -> Conj (cnf p, cnf q)
  | Disj(p,q) -> distrib (cnf p) (cnf q)
  | p -> p (* a literal *)

exception NonCNF

let rec positives prop = match prop with
    (Atom a)       -> [a]
  | (Neg (Atom _)) -> []
  | (Disj(p,q))    -> (positives p) @ (positives q)
  | _              -> raise NonCNF


let rec negatives prop = match prop with
    (Atom _)      -> []
  | (Neg(Atom a)) -> [a]
  | (Disj(p,q))   -> (negatives p) @ (negatives q)
  | _             -> raise NonCNF


let rec inter xs ys = match (xs,ys) with
    ([],ys') -> []
  | (x::xs',ys') -> 
    if (List.mem x ys) then x::(inter xs' ys') else (inter xs' ys')


let rec taut prop = match prop with
    (Conj(p,q)) -> (taut p) && (taut q)
  | p           -> (List.length (inter (positives p) (negatives p)) > 0)

let rich = Atom "rich"
and landed = Atom "landed"
and saintly = Atom "saintly"

let true_atoms = ["rich";"saintly"]

let assumption1 = implies landed rich
and assumption2 = Neg(Conj(saintly,rich))

let concl = implies landed (Neg saintly)

let goal = implies (Conj(assumption1,assumption2)) concl

let a = Atom("a")
and b = Atom("b")
and c = Atom("c")
and d = Atom("d")

let cgoal = cnf (nnf goal)
