(* Random tests of the decision procedure.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let solve_and_check phi =
  let vars = SSL.get_vars phi in
  match Api.solve phi vars with
  | `Sat sh -> ModelChecker.check sh phi
  | `Unsat -> true
  | `Unknown -> false

module Generator = ArbitrarySSL.Make
  (struct
    let n_vars = 10
    let n_selectors = (1, 1)
    let depth = (1, 1)
    let lists = true
    let unfold = false
    let star_arity = (0, 5)
  end)

let qf_symbolic_heaps_sat () =
  QCheck.Test.make
    ~count:10000
    ~name:"QF_SHLS_SAT"
    Generator.qf_symbolic_heap
    solve_and_check

let qf_symbolic_heaps_entl () =
  QCheck.Test.make
    ~count:10000
    ~name:"QF_SHLS_ENTL"
    Generator.qf_symbolic_heap_entl
    solve_and_check

let () =
  let suite1 = qf_symbolic_heaps_sat () in
  let suite2 = qf_symbolic_heaps_entl () in
  let _ = QCheck_runner.run_tests ~verbose:true [suite1; suite2] in
  ()
  (*run "Quickcheck" [
    "Decision procedure", [
      test_case "QF_SHLS_SAT"   `Quick qf_symbolic_heaps_sat;
      (*test_case "QF_SHLS_ENTL"  `Quick qf_symbolic_heaps_entl; *)
    ];
  ]
  *)
