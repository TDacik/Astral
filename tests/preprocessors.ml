(* Tests for rewritting of must-equalities
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Alcotest
open Astral_lib

open SSL.Infix

let x = SSL.Var (SSL.Variable.mk "x")
let y = SSL.Var (SSL.Variable.mk "y")
let z = SSL.Var (SSL.Variable.mk "z")

let preprocess phi =
  let sl_graph = SL_graph.compute phi in
  Simplifier.preprocess @@ EqualityRewritter.preprocess sl_graph phi

let (===) = SSL.equal

(** Tests *)

let test1 () =
  let phi = (x |-> y) * (x == y) in
  let phi' = y |-> y in
  assert (preprocess phi === phi')

let test2 () =
  let phi = (x |-> y) && (x == y) in
  let phi' = y |-> y && SSL.mk_emp () in
  assert (preprocess phi === phi')

let test3 () =
  let phi = (x |-> z) * (x == y) * (y == z) in
  let phi' = z |-> z in
  assert (preprocess phi === phi')

let test4 () =
  let phi = (x |-> y) * (y |-> z) in
  assert (preprocess phi === phi)

let () =
  run "Preprocessors" [
    "EqualityRewritter", [
      test_case "Test" `Quick test1;
      test_case "Test" `Quick test2;
      test_case "Test" `Quick test3;
      test_case "Test" `Quick test4;
    ];
  ]
