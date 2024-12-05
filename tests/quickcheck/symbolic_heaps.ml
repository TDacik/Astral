(* Random tests of the decision procedure.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let solve_and_check phi =
  let solver = Solver.init ~backend:`Z3 ~produce_models:true () in
  let phi = NegationNormalisation.apply phi in
  match Solver.solve solver phi with
  | `Sat (Some sh) ->
      if Result.get_ok @@ ModelChecker.check sh phi then true
      else (SL.print phi; StackHeapModel.print sh; exit 1)
  | `Unsat -> true
  | `Unknown _ -> false
  | `Sat None -> assert false

module Arbitrary = ArbitrarySL.Make
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
    ~print:SL.show
    Arbitrary.qf_symbolic_heap
    solve_and_check

let qf_symbolic_heaps_entl () =
  QCheck2.Test.make
    ~count:10000
    ~name:"QF_SHLS_ENTL"
    ~print:SL.show
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
