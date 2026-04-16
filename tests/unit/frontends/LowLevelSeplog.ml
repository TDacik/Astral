(* Tests for low-level separation logic.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module LL = LowLevelSeplog.Make(struct let width = 16 end)()

let x = LL.Term.mk_ptr_var "x"
let y = LL.Term.mk_ptr_var "y"

let bx = LL.Term.mk_block_begin x
let ex = LL.Term.mk_block_end x

let by = LL.Term.mk_block_begin y
let ey = LL.Term.mk_block_end y

let c16 = LL.Term.mk_const ~size:16 16
let c1600 = LL.Term.mk_const ~size:16 1600

let test_check_sat phi =
  let msg = Format.asprintf "SAT: %s" (LL.show phi) in
  Alcotest.check' Alcotest.bool ~msg ~actual:(LL.check_sat phi = `Sat) ~expected:true

let test_check_unsat phi =
  let msg = Format.asprintf "UNSAT: %s" (LL.show phi) in
  Alcotest.check' Alcotest.bool ~msg ~actual:(LL.check_sat phi = `Sat) ~expected:false

(** Sanity tests *)

let emp_sat () =
  let phi = LL.emp in
  test_check_sat phi

let distinct_unsat () =
  let phi = LL.mk_distinct [x; x] in
  test_check_unsat phi

let pointsto_from_nil () =
  let phi = LL.mk_pto LL.Term.null LL.Term.null in
  test_check_unsat phi

(** Arithmetic *)
let arithmetic_test1 () =
  let t = LL.Term.mk_plus x c16 in
  let phi = LL.mk_pto t LL.Term.null in
  test_check_sat phi

let arithmetic_test2 () =
  let t = LL.Term.mk_minus x c16 in
  let phi = LL.mk_pto t LL.Term.null in
  test_check_sat phi

let arithmetic_test3 () =
  let t = LL.Term.mk_minus x c16 in
  let phi = LL.mk_lesser LL.Term.null t in
  test_check_sat phi

let arithmetic_test4 () =
  let t = LL.Term.mk_minus x c16 in
  let phi = LL.mk_lesser_or_eq LL.Term.null t in
  test_check_sat phi

let arithmetic_test5 () =
  let t = LL.Term.mk_mult x c16 in
  let phi = LL.mk_lesser_or_eq LL.Term.null t in
  test_check_sat phi

let arithmetic_test6 () =
  let t = LL.Term.mk_mult x c16 in
  let phi = LL.mk_greater_or_eq LL.Term.null t in
  test_check_sat phi

let arithmetic_test7 () =
  let t1 = LL.Term.mk_mult x c16 in
  let t2 = LL.Term.mk_mult y c16 in
  let phi = LL.mk_star [
    LL.mk_pto t1 LL.Term.null;
    LL.mk_pto t2 LL.Term.null;
  ]
  in
  test_check_sat phi

(** Blocks *)

let block_test1 () =
  let phi = LL.mk_star [LL.mk_eq2 x y; LL.mk_distinct2 bx by] in
  test_check_unsat phi

let block_test2 () =
  let phi = LL.mk_star [LL.mk_distinct2 x y; LL.mk_eq2 bx by] in
  test_check_sat phi

let block_test3 () =
  let phi = LL.mk_star [LL.mk_distinct2 bx by; LL.mk_eq2 ex ey] in
  test_check_unsat phi

let block_test_null () =
  let phi = LL.mk_eq [
    LL.Term.null;
    LL.Term.mk_block_begin LL.Term.null;
    LL.Term.mk_block_end LL.Term.null;
  ] in
  test_check_sat phi

let block_test_empty_block () =
  let phi = LL.mk_star [
    LL.mk_distinct2 x LL.Term.null;
    LL.mk_eq2 bx ex;
  ] in
  test_check_unsat phi

(** Array pointers *)

let array_ptr_sat () =
  let phi = LL.mk_pto_array x ~size:c16 in
  test_check_sat phi

let array_ptrs_unsat () =
  let arr1 = x in
  let arr2 = LL.Term.mk_plus x c16 in
  let phi = LL.mk_star [LL.mk_pto_array arr1 ~size:c1600; LL.mk_pto_array arr2 ~size:c1600] in
  test_check_unsat phi

let array_ptrs_sat () =
  let arr1 = x in
  let arr2 = LL.Term.mk_plus x c16 in
  let phi = LL.mk_star [LL.mk_pto_array arr1 ~size:c16; LL.mk_pto_array arr2 ~size:c1600] in
  test_check_sat phi

let () =
  run "Low-level SL" [
    "Basic", [
      test_case "SAT(emp)"              `Quick emp_sat;
      test_case "UNSAT(x = x)"          `Quick distinct_unsat;
      test_case "UNSAT(null |-> null)"  `Quick pointsto_from_nil;
    ];
    "Arithmetic", [
      test_case "plus"      `Quick arithmetic_test1;
      test_case "minus"     `Quick arithmetic_test2;
      test_case "SAT(null <  x - 16)"   `Quick arithmetic_test3;
      test_case "SAT(null <= x - 16)"   `Quick arithmetic_test4;
      test_case "SAT(null <= x * 16)"   `Quick arithmetic_test5;
      test_case "SAT(null >= x * 16)"   `Quick arithmetic_test6;
      test_case "SAT(...)"              `Quick arithmetic_test7;
    ];
    "Blocks", [
      test_case "UNSAT(...)"    `Quick block_test1;
      test_case "SAT(...)"      `Quick block_test2;
      test_case "UNSAT(..)"     `Quick block_test3;
      test_case "SAT(0 = end(0) = begin(0))"        `Quick block_test_null;
      test_case "UNSAT(x != 0 * begin(x) = end(x))" `Quick block_test_empty_block;
      test_case "SAT(x -> ?[4])"  `Quick array_ptr_sat;
      test_case "SAT(x -> ?[4] * x + 4 -> ?[100])" `Quick array_ptrs_sat;
      test_case "UNSAT(x -> ?[100] * x + 1 -> ?[100])" `Quick array_ptrs_unsat;
    ];
  ]
