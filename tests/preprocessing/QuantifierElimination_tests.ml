(* Tests for quantifier elimination.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open MemoryModel

module SL = SL_testable
open SL

let apply phi =
  let g = SL_graph.compute phi in
  QuantifierElimination.apply g phi

(** Tests *)

let test1 () =
  let input = SL.mk_exists' [Sort.loc_ls] (fun [e] -> x |-> e) in
  let expected = x |-> (SL.Term.mk_heap_term Field.next x) in
  SL.check_apply apply ~input ~expected

let test2 () =
  let input = SL.mk_exists' [Sort.loc_ls] (fun [e] -> (x |-> y) * (x == e)) in
  let expected = x |-> y in
  SL.check_apply apply ~input ~expected

let test3 () =
  let input = SL.mk_exists' [Sort.loc_ls] (fun [e] -> (x |-> y) * (x != e)) in
  let expected = x |-> y in
  SL.check_apply apply ~input ~expected

let test4 () =
  let sort = Sort.mk_loc "tuple_2" in
  let input = SL.mk_exists' [sort; sort] (fun [e1; e2] -> x |=> [e1; e2]) in
  let h1 = SL.Term.mk_heap_term (Field.mk "f_0" @@ Sort.mk_loc "tuple_2") x in
  let h2 = SL.Term.mk_heap_term (Field.mk "f_1" @@ Sort.mk_loc "tuple_2") x in
  let expected = x |=> [h1; h2] in
  SL.check_apply apply ~input ~expected


let () =
  run "Quantifier elimination" [
    "apply", [
      test_case "Test" `Quick test1;
      test_case "Test" `Quick test4;
      test_case "Test" `Quick test2;
      test_case "Test" `Quick test3;
    ];
  ]
