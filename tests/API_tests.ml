(* Tests of Astral's API.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

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
    ]
  ]
