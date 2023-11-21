(* Random tests of the decision procedure.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let solve_and_check phi =
  let solver = Solver.init () in
  let phi = SSL.normalise phi in
  match Solver.solve solver phi with
  | `Sat sh ->
      if ModelChecker.check sh phi then true
      else (SSL.print phi ;StackHeapModel.print sh; exit 1)
  | `Unsat -> true
  | `Unknown _ -> false

module Arbitrary = ArbitrarySSL.Make
  (struct
    let n_vars = 10
    let n_selectors = (1, 1)
    let depth = (1, 1)
    let lists = true
    let unfold = false
    let star_arity = (0, 5)
  end)

let qf_symbolic_heaps_sat () =
  QCheck2.Test.make
    ~count:10000
    ~name:"QF_SHLS_SAT"
    ~print:SSL.show
    Arbitrary.qf_symbolic_heap
    solve_and_check

let qf_symbolic_heaps_entl () =
  QCheck2.Test.make
    ~count:10000
    ~name:"QF_SHLS_ENTL"
    ~print:SSL.show
    Arbitrary.qf_symbolic_heap_entl
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
  ]*)
