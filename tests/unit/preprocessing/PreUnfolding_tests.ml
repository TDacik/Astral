

module SL = SL_testable
open SL
open InductiveDefinition_testable

let sid =
  SID.register_user_defined SID.empty ls

let apply = PreUnfolding.unfold sid

let test1 () =
  let phi = SL.mk_predicate "ls" [x; y] ~min_depth:0 in
  SL.check_equal (apply phi) phi

let test2 () =
  let phi = SL.mk_predicate "ls" [x; y] ~min_depth:1 in
  let expected =
    SL.mk_exists'
      [Sort.loc_ls]
      (fun [n] -> SL.mk_star [SL.mk_distinct [x; y]; SL.mk_pto x n; SL.mk_predicate "ls" [n; y]])
  in
  SL.check_equal (apply phi) expected

let test3 () =
  let phi = SL.mk_predicate "ls" [x; y] ~min_depth:2 in
  let expected =
    SL.mk_exists'
      [Sort.loc_ls; Sort.loc_ls]
      (fun [n1; n2] -> SL.mk_star [
        SL.mk_distinct [x; y];
        SL.mk_distinct [n1; y];
        SL.mk_pto x n1;
        SL.mk_pto n1 n2;
        SL.mk_predicate "ls" [n2; y]])
  in
  SL.check_equal (apply phi) expected

let () =
  run "Antiprenexing" [
    "apply", [
      test_case "ls_0+" `Quick test1;
      test_case "ls_1+" `Quick test2;
      test_case "ls_2+" `Quick test3;
    ];
  ]
