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

let test3 () =
  let nx = SL.Term.mk_heap_term Field.next x in
  let input = SL.mk_or [x == y; (x != y) * (x |-> nx)] in
  SL.check_apply (IntroduceIfThenElse.apply ~forbidden_vars:[Var.x]) ~input ~expected:input

let test4 () =
  let input = SL.mk_or [(x |-> y) * (y == nil); y |-> z] in
  let expected = SL.mk_ite (y == nil) (x |-> y) (y |-> z) in
  SL.check_apply IntroduceIfThenElse.apply ~input ~expected

let test5 () =
  let input = SL.mk_or [(x == y); SL.mk_exists' [Sort.loc_ls] (fun [e] -> (x != y) * (y |-> e))] in
  let expected = SL.mk_ite (x == y) emp (SL.mk_exists' [Sort.loc_ls] (fun [e] -> y |-> e)) in
  SL.check_apply IntroduceIfThenElse.apply ~input ~expected

let test6 () =
  let input = SL.mk_or [(x |-> y) * (y == nil); y |-> z] in
  let expected = SL.mk_ite (y == nil) (x |-> y) (y |-> z) in
  SL.check_apply IntroduceIfThenElse.apply ~input ~expected

let () =
  run "Introduce if-then-else" [
    "apply", [
      test_case "Test 1" `Quick test1;
      test_case "Test 2" `Quick test2;
      test_case "Test 3" `Quick test3;
      test_case "Test 4" `Quick test4;
      test_case "Test 5" `Quick test5;
      test_case "Test 6" `Quick test6;
    ];
  ]
