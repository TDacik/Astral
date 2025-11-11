(* Tests for communication with backends.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module SMT = SMT_testable
open SMT

let apply = Backend_preprocessor.apply

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
  ]
