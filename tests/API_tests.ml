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

let () =
  run "API" [
    "check_sat", [
      test_case "sat"   `Quick check_sat_test1;
      test_case "unsat" `Quick check_sat_test2;
    ]]

