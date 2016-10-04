open Tautology

let a1 = Atom("a1")
and a2 = Atom("a2")
and a3 = Atom("a3")

let p1 = a1
and p2 = Neg(a1)
and p3 = Conj(a1,a2)
and p4 = Disj(a1,a2)

let () = 
  (** eval tests **)
  (* atom *)
  assert (not (eval p1 []));
  assert (eval p1 ["a1"]);
  (* negation *)
  assert (eval p2 []);
  assert (not (eval p2 ["a1"]));
  (* conjunction *)
  assert (not (eval p3 []));
  assert (not (eval p3 ["a1"]));
  assert (not (eval p3 ["a2"]));
  assert (eval p3 ["a1";"a2"]);
  (* disjunction *)
  assert (not (eval p4 []));
  assert (eval p4 ["a1"]);
  assert (eval p4 ["a2"]);
  assert (eval p4 ["a1";"a2"]);

  (** nnf tests **)
  assert ((nnf a1) = a1);
  assert ((nnf (Neg a1)) = (Neg a1));
  assert ((nnf (Neg (Neg a1))) = a1);
  assert ((nnf (Neg (Conj(a1,a2)))) = (Disj(Neg(a1), Neg(a2))));
  assert ((nnf (Neg (Disj(a1,a2)))) = (Conj(Neg(a1), Neg(a2))));
  assert ((nnf (Conj(a1,a2))) = (Conj(a1,a2)));
  assert ((nnf (Disj(a1,a2))) = (Disj(a1,a2)));

  (** distrib tests **)
  assert ((distrib a1 (Conj(a2,a3))) = Conj(Disj(a1,a2),Disj(a1,a3)));
  assert ((distrib (Conj(a2,a3)) a1) = Conj(Disj(a2,a1),Disj(a3,a1)));
  print_endline "Success"
