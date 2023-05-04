(* Tests of predicate unfolding functions
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Infix

let unfold = Predicate_unfolding.unfold

let x = SSL.mk_var "x"
let y = SSL.mk_var "y"
let ls = SSL.mk_ls x y


let unfold_test1 () =
  let unfolding = x == y in
  assert (unfold ls 0 === unfolding)

let unfold_test2 () =
  let unfolding0 = x == y in
  let unfolding1 = (x != y) * (x |-> y) in
  let unfolding = unfolding0 || unfolding1 in
  assert (unfold ls 1 === unfolding)

let unfold_test3 () =
  let unfolded = unfold ls 2 in
  let vars = SSL.free_vars unfolded in
  (* Variables are sorted. TODO: Remove this hack after we have alpha-equivalence in === *)
  let l = List.hd vars in
  let unfolding0 = x == y in
  let unfolding1 = (x != y) * (x |-> y) in
  let unfolding2 = (x != y) * (l != y) * (x |-> l) * (l |-> y) in
  let unfolding = (unfolding0 || unfolding1) || unfolding2 in
  assert (unfolded === unfolding)

let () =
  run "Predicate unfolding" [
    "List segments", [
      test_case "Unfold list segment 0 times" `Quick unfold_test1;
      test_case "Unfold list segment 1 times" `Quick unfold_test2;
      test_case "Unfold list segment 2 times" `Quick unfold_test3;
    ];
  ]
