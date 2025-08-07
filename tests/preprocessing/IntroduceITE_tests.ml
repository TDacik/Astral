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

let multiple_test1 () =
  let b1 = w |-> nil in
  let b2 = x |-> nil in
  let b3 = y |-> nil in
  let b4 = z |-> nil in
  let c1 = SL.mk_star [b1; x == nil; y == nil] in
  let c2 = SL.mk_star [b2; x == nil; y != nil] in
  let c3 = SL.mk_star [b3; x != nil; y == nil] in
  let c4 = SL.mk_star [b4; x != nil; y != nil] in
  let input = SL.mk_or [c1; c2; c3; c4] in
  let expected =
    SL.mk_ite (y == nil)
      (SL.mk_ite (x == nil) c1 c3)
      (SL.mk_ite (x == nil) c2 c4)
  in
  SL.check_apply IntroduceIfThenElse.apply ~input ~expected

let () =
  run "Introduce if-then-else" [
    "simple", [
      test_case "Simple 1" `Quick test1;
      test_case "Simple 2" `Quick test2;
      test_case "Simple 3" `Quick test3;
      test_case "Simple 4" `Quick test4;
      test_case "Simple 5" `Quick test5;
      test_case "Simple 6" `Quick test6;
    ];
    "multiple", [
      test_case "Multiple 1" `Quick multiple_test1;
    ];
  ]
