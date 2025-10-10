(* Predicate analysis tests.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module SL = SL_testable

open PredicateInfo.Entry

let test_pred ?(deps=[]) ~root ~allocated ~dangling ~stable ~unfolding pred =
  let sid = SID.of_list (pred :: deps) in
  let info = BoundComputation.compute_pred sid pred in
  SL.Var.check_name ~msg:"Root" info.root root;
  SL.Var.check_names ~msg:"Must-allocated parameters" info.allocated allocated;
  SL.Var.check_names ~msg:"Must-dangling parameters" info.never_allocated dangling;
  Alcotest.check' Alcotest.int ~msg:"Stable depth" ~actual:info.stable_depth ~expected:stable;
  Alcotest.check' Alcotest.int ~msg:"Unfolding depth" ~actual:info.unfolding_depth ~expected:unfolding

let test_ls () =
  test_pred Lists.ls ~root:"x" ~allocated:["x"] ~dangling:["y"] ~stable:1 ~unfolding:2

let test_dls () =
  test_pred Lists.dls ~root:"x" ~allocated:["x"; "xp"] ~dangling:["y"] ~stable:2 ~unfolding:3

let test_nls () =
  test_pred Lists.nls ~deps:[Lists.ls] ~root:"x" ~allocated:["x"] ~dangling:["y"; "z"] ~stable:1 ~unfolding:2

let test_dnls () = failwith "TODO"

let test_skl2 () = failwith "TODO"

let test_skl3 () = failwith "TODO"

let test_ls_back () =
  test_pred Lists.ls_back ~root:"x" ~allocated:["x"] ~dangling:["y"] ~stable:1 ~unfolding:2

let () =
  run "Predicate analysis" [
    "Single predicate", [
      test_case "ls"      `Quick test_ls;
      test_case "dls"     `Quick test_dls;
      test_case "nls"     `Quick test_nls;
      test_case "dnls"    `Quick test_dnls;
      test_case "skl2"    `Quick test_skl2;
      test_case "skl3"    `Quick test_skl3;
      test_case "ls_back" `Quick test_ls_back;
    ];
  ]
