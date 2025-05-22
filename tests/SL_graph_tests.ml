(* Tests for operations over SL-graphs.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open MemoryModel

module SL = SL_testable
open SL

let must_allocated_test1 () =
  let g = SL_graph.compute @@ (x |-> y) * (x == x') in
  let actual = SL_graph.must_allocated x g in
  Alcotest.check' Alcotest.bool ~msg:"" ~actual ~expected:true

let must_allocated_test2 () =
  let g = SL_graph.compute @@ (x |-> y) * (x == x') in
  let actual = SL_graph.must_allocated x' g in
  Alcotest.check' Alcotest.bool ~msg:"" ~actual ~expected:true

let must_allocated_test3 () =
  let g = SL_graph.compute @@ (x |-> y) * (x == z) * (z == x') in
  let actual = SL_graph.must_allocated x' g in
  Alcotest.check' Alcotest.bool ~msg:"" ~actual ~expected:true


let nb_allocated_test1 () =
  let g = SL_graph.compute @@ (x |-> y) * (x' |-> z) * ( x == x') in
  let actual = SL_graph.nb_allocated ~distinct:false g in
  Alcotest.check' Alcotest.int ~msg:"" ~actual ~expected:2

let nb_allocated_test2 () =
  let g = SL_graph.compute @@ (x |-> y) * (x' |-> z) * ( x == x') in
  let actual = SL_graph.nb_allocated ~distinct:true g in
  Alcotest.check' Alcotest.int ~msg:"" ~actual ~expected:1

let eval_term_test1 () =
  let g = SL_graph.compute @@ (x |-> y) * (y |-> z) in
  let actual = Option.get @@ SL_graph.eval_term g y in
  SL_testable.Term.check_equal actual y

let eval_term_test2 () =
  let g = SL_graph.compute @@ (x |-> y) * (y |-> x) in
  let nnnx = SL.Term.mk_heap_term Field.next @@
            SL.Term.mk_heap_term Field.next @@
            SL.Term.mk_heap_term Field.next x
  in
  let actual = Option.get @@ SL_graph.eval_term g nnnx in
  SL_testable.Term.check_equal actual y

(*
let eval_term_or_test1 () =
  let g = Or_graph.compute @@ ((x == y) || (x == z)) in
  let actual = Or_graph.Res.get @@ Or_graph.eval_term g x in
  SL_testable.Term.check_equal_list actual ~expected:([x; y; z]);
  assert (Or_graph.Res.is_precise @@ Or_graph.eval_term g x)

let eval_term_or_test2 () =
  let g = Or_graph.compute @@ ((x == y) || (y == z)) in
  let actual = Or_graph.eval_term g x in
  SL_testable.Term.check_equal_list (Or_graph.Res.get actual) ~expected:([x; y]);
  assert (not @@ Or_graph.Res.is_precise @@ Or_graph.eval_term g x)

(** Evaluation of predicates in SL-graphs *)
*)
let check g predicate expected =
  let msg =
    Format.asprintf "Evaluation of %s in:\n%s"
      (SL.show predicate) (SL_graph.show g)
  in
  let actual = SL_graph.eval_predicate g predicate in
  TernaryLogic_testable.check_equal ~msg actual expected
(*
let check2 or_graph predicate expected =
  let msg =
    Format.asprintf "Evaluation of %s in:\n%s"
      (SL.show predicate) (Or_graph.show or_graph)
  in
  let actual = Or_graph.eval_predicate or_graph predicate in
  TernaryLogic_testable.check_equal ~msg actual expected
*)

let eval_predicate_test1 () =
  let g = SL_graph.compute (x |-> y) in
  let predicate = SL.mk_eq [x; y] in
  check g predicate Unknown

let eval_predicate_test2 () =
  let g = SL_graph.compute @@ (x |-> y) * (y |-> x) in
  let predicate = SL.mk_eq [x; y] in
  check g predicate False;
  let predicate = SL.mk_distinct [x; y] in
  check g predicate True
(*
let eval_predicate_sid_test1 () =
  SID.register_user_defined ls;
  let id =
    InductiveDefinition.map IntroduceIfThenElse.apply ls
    |> InductiveDefinition.map (QuantifierElimination.apply SL_graph.empty)
  in
  let sid = InductiveDefinition.ID_map.of_list [("ls", id)] in
  let phi =
   SL.mk_star [
      InductiveDefinition.unfold sid id [x; y] 1;
      InductiveDefinition.unfold sid id [y; z] 1;
    ]
  in
  let g = Or_graph.compute phi in
  Or_graph.output_file "or.dot" g;
  SL.print phi;
  let n0 = x in
  let n1 = SL.Term.mk_heap_term Field.next n0 in
  let n2 = SL.Term.mk_heap_term Field.next n1 in
  let n3 = SL.Term.mk_heap_term Field.next n2 in
  check2 g (n0 == z) Unknown;
  check2 g (n1 == z) Unknown;
  check2 g (n2 == z) Unknown;
  check2 g (n3 == z) False
*)
let () =
  run "SL-graph" [
    "must_allocated", [
      test_case "Direct "      `Quick must_allocated_test1;
      test_case "Transitive 1" `Quick must_allocated_test2;
      test_case "Transitive 2" `Quick must_allocated_test3;
    ];
    "nb_allocated", [
      test_case "#allocated (distinct: false)" `Quick nb_allocated_test1;
      test_case "#allocated (distinct: true)"  `Quick nb_allocated_test2;
    ];
    "Term evaluation", [
      test_case "Eval term 1" `Quick eval_term_test1;
      test_case "Eval term 2" `Quick eval_term_test2;
(*      test_case "Eval term (complex) 1" `Quick eval_term_or_test1;
      test_case "Eval term (complex) 2" `Quick eval_term_or_test2;
    *)];
    "Predicate evaluation", [
      test_case "Eval pred 1" `Quick eval_predicate_test1;
      test_case "Eval pred 2" `Quick eval_predicate_test2;
    ];
    (*"Predicate evaluation (complex)", [
      test_case "Eval pred complex 1" `Quick eval_predicate_sid_test1;
    ]*)
  ]
