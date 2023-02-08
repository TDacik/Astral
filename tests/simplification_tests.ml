(* Tests for SSL simplifier
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL.Infix

let x = SSL.mk_var "x"
let y = SSL.mk_var "y"
let f = SSL.mk_var "f"
let l = SSL.mk_var "l"
let z = SSL.mk_var "z"

let (===) = SSL.equal

let simplify phi =
  let sl_graph = SL_graph.compute phi in
  Simplifier.simplify @@ EqualityRewritter.apply sl_graph phi

(** Equality rewritting *)

let test1 () =
  let phi = (x |-> y) * (x == y) in
  let phi' = y |-> y in
  assert (simplify phi === phi')

let test2 () =
  let phi = (x |-> y) && (x == y) in
  let phi' = y |-> y && SSL.mk_emp () in
  assert (simplify phi === phi')

let test3 () =
  let phi = (x |-> z) * (x == y) * (y == z) in
  let phi' = z |-> z in
  assert (simplify phi === phi')

let test4 () =
  let phi = (x |-> y) * (y |-> z) in
  assert (simplify phi === phi)


(** Inductive predicates *)

let test_ls () =
  let phi = (SSL.mk_ls x y) * (x == y) in
  let phi' = SSL.mk_emp () in
  assert (simplify phi = phi')

let test_dls1 () =
  let phi = (SSL.mk_dls x y f l) * (x == l) in
  let phi' = SSL.mk_emp () in
  assert (simplify phi = phi')

let test_dls2 () =
  let phi = (SSL.mk_dls x y f l) * (y == f) in
  let phi' = SSL.mk_emp () in
  assert (simplify phi = phi')


(** Guarded negations *)

let test1 () =
  let phi = SSL.mk_true () &! (x |-> y) in
  let phi' = SSL.mk_not (x |-> y) in
  assert (simplify phi === phi')

let test2 () =
  let phi = SSL.mk_false () &! (x |-> y) in
  let phi' = SSL.mk_false () in
  assert (simplify phi === phi')

let test3 () =
  let phi = (x |-> y) &! SSL.mk_true () in
  let phi' = SSL.mk_false () in
  assert (simplify phi === phi')

let test4 () =
  let phi = (x |-> y) &! SSL.mk_false () in
  let phi' = x |-> y in
  assert (simplify phi === phi')

let () =
  run "Simplification" [
    "EqualityRewritter", [
      test_case "Test" `Quick test1;
      test_case "Test" `Quick test2;
      test_case "Test" `Quick test3;
      test_case "Test" `Quick test4;
    ];
    "Inductive predicate", [
      test_case "ls(x,y) * x = y ~> emp"      `Quick test_ls;
      test_case "dls(x,y,f,l) * x = l ~> emp" `Quick test_dls1;
      test_case "dls(x,y,f,l) * y = f ~> emp" `Quick test_dls2;
    ];
    "Guarded negation", [
      test_case "Test" `Quick test1;
      test_case "Test" `Quick test2;
      test_case "Test" `Quick test3;
      test_case "Test" `Quick test4;
    ];
  ]
