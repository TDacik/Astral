(* Tests of Astral's API.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

let x = SSL.mk_var "x" Sort.loc_ls
let y = SSL.mk_var "y" Sort.loc_ls
let z = SSL.mk_var "z" Sort.loc_ls

let check_sat_test1 () =
  let solver = Solver.init () in
  let phi = SSL.mk_star [SSL.mk_ls x y; SSL.mk_ls x z] in
  assert (Solver.check_sat solver phi)

let check_sat_test2 () =
  let solver = Solver.init () in
  let phi = SSL.mk_star [SSL.mk_ls x y; SSL.mk_ls x z; SSL.mk_distinct_list [x; y; z]] in
  assert (not @@ Solver.check_sat solver phi)

let () =
  run "API" [
    "check_sat", [
      test_case "sat"   `Quick check_sat_test1;
      test_case "unsat" `Quick check_sat_test2;
    ]]

