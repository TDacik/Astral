(* Tests for operations over SL formulae
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module SL = SL_testable
open SL

(** Fragment classification TODO: entailments*)

let is_symbolic_heap_eq () =
  let phi = x == y in
  SL.check SL.is_symbolic_heap phi

let is_symbolic_heap_distinct () =
  let phi = SL.mk_distinct [x; y; z] in
  SL.check SL.is_symbolic_heap phi

let is_symbolic_heap_ptr () =
  let phi = x |-> y in
  SL.check SL.is_symbolic_heap phi

let is_symbolic_heap_star () =
  let phi = (x |-> y) * (y |~> x) in
  SL.check SL.is_symbolic_heap phi

let is_symbolic_heap_star_with_pure () =
  let phi = (x |-> y) * (x == y) in
  SL.check SL.is_symbolic_heap phi

let is_symbolic_heap_exists () =
  let phi = SL.mk_exists' [Sort.loc_ls; Sort.loc_ls] (fun [x; y] -> x |-> y) in
  SL.check SL.is_symbolic_heap phi


(** **)


let () =
  run "SL" [
    "Fragment classification", [
      test_case "Symbolic heap: x = y"            `Quick is_symbolic_heap_eq;
      test_case "Symbolic heap: distinct(x,yz)"   `Quick is_symbolic_heap_distinct;
      test_case "Symbolic heap: x -> y"           `Quick is_symbolic_heap_ptr;
      test_case "Symbolic heap: x -> y * ls(y,x)" `Quick is_symbolic_heap_star;
      test_case "Symbolic heap: x -> y * x = y"   `Quick is_symbolic_heap_star_with_pure;
      test_case "Symbolic heap: E x y. x -> y"    `Quick is_symbolic_heap_exists;
    ];
  ]
