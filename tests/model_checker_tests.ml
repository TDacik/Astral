(* Tests for model checker
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL.Infix
open StackHeapModel

module SH = StackHeapModel

let x = SSL.Variable.mk "x"
let y = SSL.Variable.mk "y"

let phi_x = SSL.mk_var "x"
let phi_y = SSL.mk_var "y"

(** Tests *)

let symbolic_heap_sat_test1 () =
  let phi = phi_x |-> phi_y in
  let stack = Stack.add x 1 @@ Stack.add y 2 @@ Stack.empty in
  let heap = Heap.add 1 [2] Heap.empty in
  let sh = SH.init stack heap in
  assert (ModelChecker.check sh phi)

let symbolic_heap_sat_test2 () =
  let phi = phi_x == phi_y in
  let stack = Stack.add x 1 @@ Stack.add y 1 @@ Stack.empty in
  let heap = Heap.add 1 [1] Heap.empty in
  let sh = SH.init stack heap in
  assert (not @@ ModelChecker.check sh phi)

let symbolic_heap_sat_test3 () =
  let phi = (phi_x |-> phi_y) * (phi_x != phi_y) in
  let stack = Stack.add x 1 @@ Stack.add y 2 @@ Stack.empty in
  let heap = Heap.add 1 [2] Heap.empty in
  let sh = SH.init stack heap in
  assert (ModelChecker.check sh phi)

let symbolic_heap_entl_test1 () =
  let phi = (phi_x |~> phi_y) &! (phi_x |-> phi_y) in
  let stack = Stack.add x 1 @@ Stack.add y 3 @@ Stack.empty in
  let heap = Heap.add 1 [2] @@ Heap.add 2 [3] @@ Heap.empty in
  let sh = SH.init stack heap in
  assert (ModelChecker.check sh phi)

let symbolic_heap_entl_test2 () =
  let phi = (phi_x |~> phi_y) &! (phi_x |-> phi_y) in
  let stack = Stack.add x 1 @@ Stack.add y 3 @@ Stack.empty in
  let heap = Heap.add 1 [3] @@ Heap.empty in
  let sh = SH.init stack heap in
  assert (not @@ ModelChecker.check sh phi)


let () =
  run "SSL" [
    "symbolic heap satisfiability", [
      test_case "Test"  `Quick symbolic_heap_sat_test1;
      test_case "Test"  `Quick symbolic_heap_sat_test2;
      test_case "Test"  `Quick symbolic_heap_sat_test3;
    ];
    "symbolic heap entailment", [
      test_case "Test"  `Quick symbolic_heap_entl_test1;
      test_case "Test"  `Quick symbolic_heap_entl_test2;
    ];
  ]

