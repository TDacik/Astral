(* Tests for list utilities
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Alcotest
open Astral_lib

(** Tests *)

let all_equal1 () = assert (List_utils.all_equal (=) [])
let all_equal2 () = assert (List_utils.all_equal (=) [1])
let all_equal3 () = assert (List_utils.all_equal (=) [1; 1; 1])

let all_equal4 () = assert (not @@ List_utils.all_equal (=) [1; 2])
let all_equal5 () = assert (not @@ List_utils.all_equal (=) [1; 1; 1; 2])


let all_distinct1 () = assert (List_utils.all_distinct (=) [])
let all_distinct2 () = assert (List_utils.all_distinct (=) [1])
let all_distinct3 () = assert (List_utils.all_distinct (=) [1; 2])
let all_distinct4 () = assert (List_utils.all_distinct (=) [4; 2; 3; 1])

let all_distinct5 () = assert (not @@ List_utils.all_distinct (=) [1; 1])
let all_distinct6 () = assert (not @@ List_utils.all_distinct (=) [1; 2; 3; 1])

let diagonal_product1 () = assert (List_utils.diagonal_product [] = [])
let diagonal_product2 () = assert (List_utils.diagonal_product [1] = [])
let diagonal_product3 () = assert (List_utils.diagonal_product [1; 2] = [(1, 2)])

let () =
  run "List utilities" [
    "all_equal", [
      test_case "Test" `Quick all_equal1;
      test_case "Test" `Quick all_equal2;
      test_case "Test" `Quick all_equal3;
      test_case "Test" `Quick all_equal4;
      test_case "Test" `Quick all_equal5;
    ];
    "all_distinct", [
      test_case "Test" `Quick all_distinct1;
      test_case "Test" `Quick all_distinct2;
      test_case "Test" `Quick all_distinct3;
      test_case "Test" `Quick all_distinct4;
      test_case "Test" `Quick all_distinct5;
      test_case "Test" `Quick all_distinct6;
    ];
    "diagonal_product", [
      test_case "Empty" `Quick diagonal_product1;
      test_case "One element" `Quick diagonal_product2;
      test_case "Two elements" `Quick diagonal_product3;
    ];
  ]
