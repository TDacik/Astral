(* Tests for antiprenexing
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module SL = SL_testable
open SL

let apply = Antiprenexing.apply

(** Tests *)

let test1 () =
  let phi = SL.mk_exists' [Sort.loc_ls] (fun [x] -> SL.mk_star [x |-> x; x |-> x]) in
  SL.check_equal (apply phi) phi

let test2 () =
  let phi = SL.mk_exists' [Sort.loc_ls] (fun [x] -> SL.mk_star [x |-> x; y |-> y]) in
  let phi' = SL.mk_star [y |-> y; SL.mk_exists' [Sort.loc_ls] (fun [x] -> x |-> x)] in
  SL.check_equal (apply phi) phi'

let () =
  run "Antiprenexing" [
    "apply", [
      test_case "Test" `Quick test1;
      test_case "Test" `Quick test2;
    ];
  ]
