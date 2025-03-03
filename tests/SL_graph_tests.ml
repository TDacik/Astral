(* Tests for operations over SL-graphs.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open MemoryModel

module SL = SL_testable
open SL

let eval_term_test1 () =
  let g = SL_graph.compute @@ (x |-> y) * (y |-> z) in
  let actual = SL_graph.eval_term g y in
  SL_testable.Term.check_equal actual y

let eval_term_test2 () =
  let g = SL_graph.compute @@ (x |-> y) * (y |-> x) in
  let nnnx = SL.Term.mk_heap_term Field.next @@
            SL.Term.mk_heap_term Field.next @@
            SL.Term.mk_heap_term Field.next x
  in
  let actual = SL_graph.eval_term g nnnx in
  SL_testable.Term.check_equal actual y

let eval_predicate_test1 () =
  let g = SL_graph.compute (x |-> y) in
  let predicate = SL.mk_eq [x; y] in
  assert (SL_graph.eval_predicate g predicate = None)

let eval_predicate_test2 () =
  let g = SL_graph.compute @@ (x |-> y) * (y |-> x) in
  let predicate = SL.mk_eq [x; y] in
  assert (SL_graph.eval_predicate g predicate = Some false);
  let predicate = SL.mk_distinct [x; y] in
  assert (SL_graph.eval_predicate g predicate = Some true)

let () =
  run "SL-graph" [
    "Term evaluation", [
      test_case "Eval term 1" `Quick eval_term_test1;
      test_case "Eval term 2" `Quick eval_term_test2;
    ];
    "Predicate evaluation", [
      test_case "Eval pred 1" `Quick eval_predicate_test1;
      test_case "Eval pred 2" `Quick eval_predicate_test2;
    ];
  ]
