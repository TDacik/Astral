(* Tests for introduction of if-then-else constructions in IDs.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module SL = SL_testable
open SL

open MemoryModel

let test1 () =
  let input = SL.mk_or [x == y; (x != y) * (x |-> y)] in
  let expected = SL.mk_ite (x == y) (emp) (x |-> y) in
  SL.check_apply IntroduceIfThenElse.apply ~input ~expected

let test2 () =
  let nx = SL.Term.mk_heap_term Field.next x in
  let input = SL.mk_or [x == y; (x != y) * (x |-> nx)] in
  let expected = SL.mk_ite (x == y) (emp) (x |-> nx) in
  SL.check_apply IntroduceIfThenElse.apply ~input ~expected


let () =
  run "Introduce if-then-else" [
    "apply", [
      test_case "Test 1" `Quick test1;
      test_case "Test 2" `Quick test2;
    ];
  ]
