(* Tests of Astral's API.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open SL_builtins
open SL_testable

let check_sat_test1 () =
  let solver = Solver.init () in
  let phi = SL.mk_star [SL_builtins.mk_ls x ~sink:y; SL_builtins.mk_ls x ~sink:nil] in
  assert (Solver.check_sat solver phi)

let check_sat_test2 () =
  let solver = Solver.init () in
  let phi = SL.mk_star [
    SL_builtins.mk_ls x ~sink:y;
    SL_builtins.mk_ls x ~sink:z;
    SL.mk_distinct [x; y; z]]
  in
  assert (not @@ Solver.check_sat solver phi)

let corner_case_test1 () =
  let solver = Solver.init () in
  let phi = SL_builtins.mk_ls nil ~sink:nil in
  assert (Solver.check_sat solver phi)

(** DLS *)

let root = SL.Term.mk_var "root" loc_dls
let sink = SL.Term.mk_var "sink" loc_dls
let root' = SL.Term.mk_var "root_" loc_dls
let sink' = SL.Term.mk_var "sink_" loc_dls

let dls_test1 () =
  let solver = Solver.init () in
  let phi = SL.mk_star [
    SL.mk_distinct [root; root'; sink; sink'];
    mk_dls root ~sink ~root' ~sink';
    mk_pto_dls root ~next:nil ~prev:nil;
    mk_pto_dls root' ~next:nil ~prev:nil;]
  in
  assert (not @@ Solver.check_sat solver phi)

let dls_test2 () =
  let solver = Solver.init () in
  let phi = mk_dls nil ~root':nil ~sink:nil ~sink':nil in
  assert (Solver.check_sat solver phi)

(** NLS *)

let root = SL.Term.mk_var "root" loc_nls
let sink = SL.Term.mk_var "sink" loc_nls
let bottom = SL.Term.mk_var "bottom" loc_nls

let nls_test1 () =
  let solver = Solver.init () in
  let phi = mk_nls root ~sink ~bottom in
  assert (Solver.check_sat solver phi)

let nls_test2 () =
  let solver = Solver.init () in
  let phi = mk_nls nil ~sink:nil ~bottom:nil in
  assert (Solver.check_sat solver phi)


(** Output *)

let debug_input () =
  let dirname = Format.asprintf "%s/_astral_test" (Filename.get_temp_dir_name ()) in
  let solver = Solver.init ~dump_queries:(`Full dirname) () in
  let phi = SL.emp in
  let _ = Solver.check_sat solver phi in
  assert (Sys.file_exists @@ dirname ^ "/query_0001/input.smt2")

let () =
  run "API" [
    "debug", [
      test_case "input"   `Quick debug_input;
    ];
    "check_sat", [
      test_case "sat"   `Quick check_sat_test1;
      test_case "unsat" `Quick check_sat_test2;
    ];
    "LS", [
      test_case "ls(nil, nil)" `Quick corner_case_test1;
    ];
    "DLS", [
      test_case "DLS - sat"    `Quick dls_test1;
      test_case "dls(nil, nil, nil, nil)" `Quick dls_test2;
    ];
    "NLS", [
      test_case "nls(x, y, z)"       `Quick nls_test1;
      test_case "nls(nil, nil, nil)" `Quick nls_test2;
    ];


  ]
