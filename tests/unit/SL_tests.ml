(* Tests for operations over SL formulae
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module SL = SL_testable
open SL

(** Fragment classification TODO: entailments*)

let is_symbolic_heap_test1 () =
  let phi = x == y in
  SL.check SL.is_symbolic_heap phi

let is_symbolic_heap_test2 () =
  let phi = SL.mk_distinct [x; y; z] in
  SL.check SL.is_symbolic_heap phi

let is_symbolic_heap_test3 () =
  let phi = x |-> y in
  SL.check SL.is_symbolic_heap phi

let is_symbolic_heap_test4 () =
  let phi = (x |-> y) * (y |~> x) in
  SL.check SL.is_symbolic_heap phi

let is_symbolic_heap_test5 () =
  let phi = (x |-> y) * (x == y) in
  SL.check SL.is_symbolic_heap phi

let is_symbolic_heap_test6 () =
  let phi = (x == y) && (x == z) in
  SL.check SL.is_symbolic_heap phi

let is_symbolic_heap_test7 () =
  let phi = SL.mk_exists' [Sort.loc_ls; Sort.loc_ls] (fun [x; y] -> x |-> y) in
  SL.check SL.is_symbolic_heap phi


(** **)

let check input expected =
  let actual = SL.as_quantified_symbolic_heap input in
  let module T =
    struct
      type t = SL.Variable.t list * SL.t list
      let equal (xs1, ys1) (xs2, ys2) =
        let open Stdlib in
        List.equal SL.Variable.equal xs1 xs2
        && List.equal SL.equal ys1 ys2
      let pp fmt (xs, ys) =
        Format.fprintf fmt "%s, %s"
          (SL.Variable.show_list xs)
          (SL.show_list ys)
    end
  in
  Alcotest.check' (module T) ~msg:"" ~actual ~expected

let as_symbolic_heap_test1 () =
  let phi = x |-> y in
  check phi ([], [phi])

let as_symbolic_heap_test2 () =
  let e = SL.Variable.mk "e" Sort.loc_ls in
  let psi = x |-> (SL.Term.of_var e) in
  let phi = SL.mk_exists [e] psi in
  check phi ([e], [psi])

let () =
  run "SL" [
    "Fragment classification", [
      test_case "Symbolic heap: x = y"            `Quick is_symbolic_heap_test1;
      test_case "Symbolic heap: distinct(x,yz)"   `Quick is_symbolic_heap_test2;
      test_case "Symbolic heap: x -> y"           `Quick is_symbolic_heap_test3;
      test_case "Symbolic heap: x -> y * ls(y,x)" `Quick is_symbolic_heap_test4;
      test_case "Symbolic heap: x -> y * x = y"   `Quick is_symbolic_heap_test5;
      test_case "Symbolic heap: x = y /\ x = y"   `Quick is_symbolic_heap_test6;
      test_case "Symbolic heap: E x y. x -> y"    `Quick is_symbolic_heap_test7;
    ];

    "As symbolic heap", [
      test_case "x -> y"       `Quick as_symbolic_heap_test1;
      test_case "E e. x -> e"  `Quick as_symbolic_heap_test2;
    ];
  ]
