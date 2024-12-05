(* Tests for model checker
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SL.Infix
open StackHeapModel

module SH = StackHeapModel

let x = SL.Term.mk_var "x" Sort.loc_ls
let y = SL.Term.mk_var "y" Sort.loc_ls

let loc1 = Location.mk_ls 1
let loc2 = Location.mk_ls 2
let loc3 = Location.mk_ls 3

let val1 = Value.mk_struct LS.struct_ls [loc1]
let val2 = Value.mk_struct LS.struct_ls [loc2]
let val3 = Value.mk_struct LS.struct_ls [loc3]

let check sh phi =
  let msg = Format.asprintf "Model: TODO, formula: %s" (SL.show phi) in
  let actual = Result.get_ok (ModelChecker.check sh phi) in
  Alcotest.check' Alcotest.bool ~msg ~actual ~expected:true

let check_not sh phi =
  let msg = Format.asprintf "Model: TODO, formula: %s" (SL.show phi) in
  let actual = Result.get_ok (ModelChecker.check sh phi) in
  Alcotest.check' Alcotest.bool ~msg ~actual ~expected:false


(** Tests *)

let symbolic_heap_sat_test1 () =
  let phi = x |-> y in
  let stack = Stack.add x loc1 @@ Stack.add y loc2 @@ Stack.empty in
  let heap = Heap.add loc2 val1 Heap.empty in
  let sh = SH.init stack heap in
  check sh phi

let symbolic_heap_sat_test2 () =
  let phi = x == y in
  let stack = Stack.add x loc1 @@ Stack.add y loc1 @@ Stack.empty in
  let heap = Heap.add loc1 val1 Heap.empty in
  let sh = SH.init stack heap in
  check_not sh phi

let symbolic_heap_sat_test3 () =
  let phi = (x |-> y) * (x != y) in
  let stack = Stack.add x loc1 @@ Stack.add y loc2 @@ Stack.empty in
  let heap = Heap.add loc1 val2 Heap.empty in
  let sh = SH.init stack heap in
  check sh phi

let symbolic_heap_entl_test1 () =
  let phi = (x |~> y) &! (x |-> y) in
  let stack = Stack.add x loc1 @@ Stack.add y loc3 @@ Stack.empty in
  let heap = Heap.add loc1 val2 @@ Heap.add loc2 val3 @@ Heap.empty in
  let sh = SH.init stack heap in
  check sh phi

let symbolic_heap_entl_test2 () =
  let phi = (x |~> y) &! (x |-> y) in
  let stack = Stack.add x loc1 @@ Stack.add y loc3 @@ Stack.empty in
  let heap = Heap.add loc1 val3 @@ Heap.empty in
  let sh = SH.init stack heap in
  check_not sh phi


let () =
  run "SL" [
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
