(* Tests for computation of small models.
 *
 * TODO: skl(2) |= skl1
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025

open PredicateAbstraction

open SL_testable
open InductiveDefinition_testable

let init predicates =
  SID.reset ();
  List.iter SID.register_user_defined predicates;
  DependencyGraph.compute ()

let check res pred expected =
  let actual = (PredicateAbstraction.find pred res).fixpoint_size in
  let precision = 0.0 in
  let msg = Format.asprintf "Computed bound does not match (with precision %f)" precision in
  Alcotest.check' (Alcotest.float precision) ~msg ~actual ~expected


let test_ls1 () =
  let g = init [ls] in
  let phi = SL.mk_gneg (SL.mk_pto x y) (InductiveDefinition.mk_call ls [x; y]) in
  let res = SmallModels.compute g phi in
  check res ls 1.0

let test_ls2 () =
  let g = init [ls] in
  let phi = SL.mk_gneg SL.emp (SL.mk_pto x y) in
  let res = SmallModels.compute g phi in
  check res ls 2.0

let test_dls1 () =
  let g = init [dls] in
  let phi = SL.mk_gneg (SL.mk_pto x y) (InductiveDefinition.mk_call dls [x; y; nil; nil]) in
  let res = SmallModels.compute g phi in
  check res dls 1.0

let test_dls2 () =
  let g = init [dls] in
  let phi = SL.mk_gneg (InductiveDefinition.mk_call dls [x; y; nil; nil]) (SL.mk_pto x y) in
  let res = SmallModels.compute g phi in
  check res dls 1.5

let test_dnls1 () =
  let g = init [ls; dnls] in
  let phi = SL.mk_gneg (SL.mk_pto x y) (InductiveDefinition.mk_call dnls [x; y; nil; nil; nil]) in
  let res = SmallModels.compute g phi in
  check res dnls 1.0

let test_dnls2 () =
  let g = init [ls; dnls] in
  let phi = SL.mk_gneg (InductiveDefinition.mk_call dnls [x; y; nil; nil; nil]) (SL.mk_star [SL.mk_pto x y; SL.mk_pto y x]) in
  let res = SmallModels.compute g phi in
  check res dnls 1.5

let () =
  run "Small models" [
    "Compute bound", [
      test_case "ls (no ptr) ~> 1"      `Quick test_ls1;
      test_case "ls (with ptr) ~> 2"    `Quick test_ls2;
      test_case "dls (no ptr) ~> 1.5"     `Quick test_dls1;
      test_case "dls (with ptr) ~> 1"   `Quick test_dls2;
      test_case "dnls (no ptr) ~> 1"    `Quick test_dnls1;
      test_case "dnls (no ptr) ~> 1.5"    `Quick test_dnls2;
    ];
  ]
*)
