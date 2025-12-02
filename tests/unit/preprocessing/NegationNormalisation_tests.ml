(* Tests for negation normalisation.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module SL = SL_testable
open SL

let check_apply = SL.check_apply NegationNormalisation.apply

let test1 () =
  let lhs = SL.mk_pto x x in
  let rhs = SL.mk_pto y y in
  let input = SL.mk_and [lhs; SL.mk_not rhs] in
  let expected = SL.mk_gneg lhs rhs in
  check_apply ~input ~expected

let test2 () =
  let psi1 = SL.mk_pto x x in
  let psi2 = SL.mk_pto y y in
  let psi3 = SL.mk_pto z z in
  let input = SL.mk_and [psi1; psi2; SL.mk_not psi3] in
  let expected = SL.mk_gneg (SL.mk_and [psi1; psi2]) psi3 in
  check_apply ~input ~expected

let () =
  run "Negation normalisation" [
    "apply", [
      test_case "Binary" `Quick test1;
      test_case "N-anary" `Quick test2;
    ];
  ]
