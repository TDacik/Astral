(* Tests for SSL simplifier
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Infix

let x = SSL.mk_var "x" Sort.loc_ls
let y = SSL.mk_var "y" Sort.loc_ls
let f = SSL.mk_var "f" Sort.loc_ls
let l = SSL.mk_var "l" Sort.loc_ls
let z = SSL.mk_var "z" Sort.loc_ls


(** Equality rewritting *)

let simplify phi =
  let sl_graph = SL_graph.compute phi in
  Simplifier.simplify @@ EqualityRewritter.apply sl_graph phi

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

let simplify phi =
  let sl_graph = SL_graph.compute phi in
  Simplifier.simplify @@ EqualityRewritter.apply sl_graph phi

let test_ls () =
  let phi = (SSL.mk_ls x y) * (x == y) in
  let phi' = SSL.mk_emp () in
  assert (simplify phi === phi')

let test_dls1 () =
  let phi = (SSL.mk_dls x y f l) * (x == l) in
  let phi' = y == f in
  assert (simplify phi === phi')

let test_dls2 () =
  let phi = (SSL.mk_dls x y f l) * (y == f) in
  let phi' = x == l in
  assert (simplify phi === phi')

let test_dls3 () =
  let phi = (SSL.mk_dls x y f l) * (x == l) * (y == f) in
  let phi' = SSL.mk_emp () in
  assert (simplify phi === phi')


(** Guarded negations *)

let simplify = Simplifier.simplify

let gneg_test1 () =
  let phi = SSL.mk_true () &! (x |-> y) in
  let phi' = SSL.mk_not (x |-> y) in
  assert (simplify phi === phi')

let gneg_test2 () =
  let phi = SSL.mk_false () &! (x |-> y) in
  let phi' = SSL.mk_false () in
  assert (simplify phi === phi')

let gneg_test3 () =
  let phi = (x |-> y) &! SSL.mk_true () in
  let phi' = SSL.mk_false () in
  assert (simplify phi === phi')

let gneg_test4 () =
  let phi = (x |-> y) &! SSL.mk_false () in
  let phi' = x |-> y in
  assert (simplify phi === phi')


(** Stars *)

let simplify = Simplifier.simplify

let star_fold_test1 () =
  let phi = (x |-> y) * (y |-> x) in
  assert (simplify phi === phi)

let star_fold_test2 () =
  let psi1 = (x |-> y) * (y |-> x) in
  let psi2 = (f |-> l) * (l |-> f) in
  let phi = psi1 * psi2 in
  let phi' = SSL.mk_star [x |-> y; y |-> x; f |-> l ; l |-> f] in
  assert (simplify phi === phi')

let star_fold_test3 () =
  let psi1 = x |-> y in
  let psi2 = y |-> x in
  let psi3 = f |-> l in
  let psi4 = l |-> f in
  let phi = (psi1 * (psi2 * psi3)) &! psi4 in
  let phi' = (SSL.mk_star [psi1; psi2; psi3]) &! psi4 in
  assert (simplify phi === phi')

let star_pure_duplicates_test1 () =
  let phi = (f |-> z) * (x == y) in
  assert (simplify phi === phi)

let star_pure_duplicates_test2 () =
  let phi = (x == y) * (x == y) in
  let phi' = (x == y) in
  assert (simplify phi === phi')

let star_simplify_test1 () =
  let psi = SSL.mk_not @@ SSL.mk_emp () in
  let phi = SSL.mk_star [psi; psi] in
  Printf.printf "%s ~> %s\n" (SSL.show phi) (SSL.show @@ simplify phi);
  assert (simplify phi === phi)

let () =
  run "Simplification" [
    "EqualityRewritter", [
      test_case "Test" `Quick test1;
      test_case "Test" `Quick test2;
      test_case "Test" `Quick test3;
      test_case "Test" `Quick test4;
    ];
    (*"Inductive predicate", [
      test_case "ls(x,y) * x = y ~> emp"              `Quick test_ls;
      test_case "dls(x,y,f,l) * x = l ~> y = f"       `Quick test_dls1;
      test_case "dls(x,y,f,l) * y = f ~> x = l"       `Quick test_dls2;
      test_case "dls(x,y,f,l) * x = l * y = f ~> emp" `Quick test_dls3;
    ];*)
    "Guarded negation", [
      test_case "Test" `Quick gneg_test1;
      test_case "Test" `Quick gneg_test2;
      test_case "Test" `Quick gneg_test3;
      test_case "Test" `Quick gneg_test4;
    ];
    "Stars", [
      test_case "Test" `Quick star_fold_test1;
      test_case "Test" `Quick star_fold_test2;
      test_case "Test" `Quick star_fold_test3;
      test_case "Test" `Quick star_pure_duplicates_test1;
      test_case "Test" `Quick star_pure_duplicates_test2;
      test_case "Test" `Quick star_simplify_test1;
    ];
  ]
