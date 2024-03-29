(* Tests for Broom preprocessor.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Infix

let x = SSL.mk_var "x" Sort.loc_ls
let y = SSL.mk_var "y" Sort.loc_ls
let z = SSL.mk_var "z" Sort.loc_ls

let apply phi =
  let g = SL_graph.compute phi in
  BroomPreprocessing.apply g phi

(** Tests *)

let test1 () =
  let phi = SSL.mk_iff [x |-> y; y |-> x] in
  assert (apply phi === phi)

let test2 () =
  let phi = SSL.mk_iff [SSL.mk_pure @@ SMT.Boolean.mk_true (); x |-> y] in
  let phi' = x |-> y in
  assert (apply phi === phi')

let () =
  run "Broom preprocessor" [
    "remove_iffs", [
      test_case "Test" `Quick test1;
      test_case "Test" `Quick test2;
    ];
  ]
