(* Tests
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module SL = SL_testable
open SL

let is_imprecise_sh_test1 () =
  let phi = SL.mk_star [SL.mk_pto x y; SL_builtins.mk_ls x ~sink:y] in
  SL.check PreciseToImprecise.is_imprecise_sh phi

let is_imprecise_sh_test2 () =
  let phi = SL.mk_and [SL.emp; SL.mk_star [SL.mk_pto x y; SL_builtins.mk_ls x ~sink:y]] in
  SL.check PreciseToImprecise.is_imprecise_sh phi

(** Precise -> imprecise *)

let to_imprecise = PreciseToImprecise.to_imprecise

let to_imprecise_test1 () =
  let phi = x |-> y in
  SL.check_equal (to_imprecise phi) phi

let to_imprecise_test2 () =
  let phi = x == y in
  let phi' = (x == y) && SL.emp in
  SL.check_equal (to_imprecise phi) phi'

let to_imprecise_test3 () =
  let phi = (x == y) * (x |-> z) in
  let phi' = (x == y) && (x |-> z) in
  SL.check_equal (to_imprecise phi) phi'

let to_imprecise_test4 () =
  let phi = SL.mk_star [x |-> y; y |-> x; x == y; y == z] in
  let phi' = ((x == y) && (y == z)) && ((x |-> y) * (y |-> x)) in
  SL.check_equal (to_imprecise phi) phi'


(** Imprecise -> precise *)

let to_precise phi =
  PreciseToImprecise.to_precise phi
  |> Simplifier.simplify

let to_precise_test1 () =
  let expected = x |-> y in
  let actual = to_precise expected in
  SL.check_equal actual expected

let to_precise_test2 () =
  let phi = x == y in
  let phi' = (x == y) * SL.tt in
  SL.check_equal (to_precise phi) phi'

let to_precise_test3 () =
  let pure = SL.mk_and [x == y; y == nil] in
  let spatial = SL.emp in
  let phi = SL.mk_and [pure; spatial] in
  let phi' = SL.mk_star [x == y; y == nil] in
  SL.check_equal (to_precise phi) phi'

let to_precise_test4 () =
  let spatial = SL.mk_star [x |-> y; y |-> x] in
  let pure = SL.mk_and [x == y; y == nil] in
  let phi = SL.mk_and [spatial; pure] in
  let phi' = SL.mk_star [x |-> y; y |-> x; x == y; y == nil] in
  SL.check_equal (to_precise phi) phi'

let to_precise_test5 () =
  let phi = SL.mk_and [x == y; y == nil] in
  let expected = SL.mk_star [x == y; y == nil] in
  SL.check_equal (to_precise phi) expected

(** Pure terms
let apply = PurePreprocessing.apply

let pure_test1 () =
  let b = SMT.Boolean.mk_var "b" in
  let phi = SL.mk_and [SL.mk_pure b; SL.mk_pure b] in
  let phi' = SL.mk_pure @@ SMT.Boolean.mk_and [b; b] in
  assert (apply phi === phi')

let pure_test2 () =
  let phi = SL.mk_not @@ SL.mk_pure @@ SMT.Boolean.mk_true () in
  let phi' = SL.mk_pure @@ SMT.Boolean.mk_false () in
  assert (apply phi === phi')
*)

let () =
  run "Preprocessors" [
    "is_imprecise_sh", [
      test_case "Test" `Quick is_imprecise_sh_test1;
      test_case "Test" `Quick is_imprecise_sh_test2;
    ];
    "Precise -> Imprecise", [
      test_case "Test" `Quick to_imprecise_test1;
      test_case "Test" `Quick to_imprecise_test2;
      test_case "Test" `Quick to_imprecise_test3;
      test_case "Test" `Quick to_imprecise_test4;
    ];
    "Imprecise -> Precise", [
      test_case "Test" `Quick to_precise_test1;
      test_case "Test" `Quick to_precise_test2;
      test_case "Test" `Quick to_precise_test3;
      test_case "Test" `Quick to_precise_test4;
      test_case "Test" `Quick to_precise_test5;
    ];
    (*"TODO: Pure preprocessing", [
      test_case "Test" `Quick pure_test1;
      test_case "Test" `Quick pure_test2;
    ];*)
  ]
