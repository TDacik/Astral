(* Tests for communication with backends.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module SMT = SMT_testable
open SMT

let apply = Backend_preprocessor.apply

(** Name mangling *)

let check same v1 v2 =
  let name1 = BackendUtils.var_unique_name v1 in
  let name2 = BackendUtils.var_unique_name v2 in
  let should_msg = if same then "should be equal" else "should_be_distinct" in
  let msg =
    Format.asprintf "Names %s:\n  %s --> %s\n  %s --> %s"
      should_msg
      (SMT.Variable.show_with_sort v1) name1
      (SMT.Variable.show_with_sort v2) name2
  in
  Alcotest.check' Alcotest.bool ~msg ~actual:(String.equal name1 name2) ~expected:same

let check_eq = check true
let check_neq = check false

let name_mangling_test1 () =
  let x = SMT.Variable.mk "x" Sort.int in
  check_eq x x

let name_mangling_test2 () =
  let x1 = SMT.Variable.mk "x" Sort.int in
  let x2 = SMT.Variable.mk "x" Sort.bool in
  check_neq x1 x2

(** Preprocessing *)

let simplify_disjoint_test1 () =
  let phi = Sets.mk_disjoint [s1; s2] in
  SMT.check_apply apply ~input:phi ~expected:phi

let simplify_disjoint_test2 () =
  let input = Sets.mk_disjoint [s1; s2; s3] in
  let expected =
    Boolean.mk_and [
      Sets.mk_disjoint [s1; s2];
      Sets.mk_disjoint [Sets.mk_union set_sort [s1; s2]; s3];
    ]
  in
  SMT.check_apply apply ~input ~expected

let () =
  run "SMT" [
    "simplify_disjoint", [
      test_case "Test"  `Quick simplify_disjoint_test1;
      test_case "Test"  `Quick simplify_disjoint_test2;
    ];
    "name_mangling", [
      test_case "same var"    `Quick name_mangling_test1;
      test_case "same name"   `Quick name_mangling_test2;
    ];
  ]
