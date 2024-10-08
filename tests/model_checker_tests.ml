(* Tests for model checker
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL.Infix
open StackHeapModel

module SH = StackHeapModel

let x = SSL.Variable.mk "x" Sort.loc_ls
let y = SSL.Variable.mk "y" Sort.loc_ls

let phi_x = SSL.mk_var "x" Sort.loc_ls
let phi_y = SSL.mk_var "y" Sort.loc_ls

let loc1 = Location.mk_ls 1
let loc2 = Location.mk_ls 2
let loc3 = Location.mk_ls 3

let val1 = Value.mk_ls loc1
let val2 = Value.mk_ls loc2
let val3 = Value.mk_ls loc3

(** Tests *)

let symbolic_heap_sat_test1 () =
  let phi = phi_x |-> phi_y in
  let stack = Stack.add x loc1 @@ Stack.add y loc2 @@ Stack.empty in
  let heap = Heap.add loc2 val1 Heap.empty in
  let sh = SH.init stack heap in
  assert (ModelChecker.check sh phi)

let symbolic_heap_sat_test2 () =
  let phi = phi_x == phi_y in
  let stack = Stack.add x loc1 @@ Stack.add y loc1 @@ Stack.empty in
  let heap = Heap.add loc1 val1 Heap.empty in
  let sh = SH.init stack heap in
  assert (not @@ ModelChecker.check sh phi)

let symbolic_heap_sat_test3 () =
  let phi = (phi_x |-> phi_y) * (phi_x != phi_y) in
  let stack = Stack.add x loc1 @@ Stack.add y loc2 @@ Stack.empty in
  let heap = Heap.add loc1 val2 Heap.empty in
  let sh = SH.init stack heap in
  assert (ModelChecker.check sh phi)

let symbolic_heap_entl_test1 () =
  let phi = (phi_x |~> phi_y) &! (phi_x |-> phi_y) in
  let stack = Stack.add x loc1 @@ Stack.add y loc3 @@ Stack.empty in
  let heap = Heap.add loc1 val2 @@ Heap.add loc2 val3 @@ Heap.empty in
  let sh = SH.init stack heap in
  assert (ModelChecker.check sh phi)

let symbolic_heap_entl_test2 () =
  let phi = (phi_x |~> phi_y) &! (phi_x |-> phi_y) in
  let stack = Stack.add x loc1 @@ Stack.add y loc3 @@ Stack.empty in
  let heap = Heap.add loc1 val3 @@ Heap.empty in
  let sh = SH.init stack heap in
  assert (not @@ ModelChecker.check sh phi)


let () = ()
(*
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
*)
