(* Tests for list utilities
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

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


let split3_test1 () = assert (List_utils.split3 [] = ([], [], []))
let split3_test2 () = assert (List_utils.split3 [(1, 2, 3)] = ([1], [2], [3]))
let split3_test3 () =
  assert (List_utils.split3 [(1, 2, 1); (2, 1, 3)] = ([1; 2], [2; 1], [1; 3]))

(** map3 *)

let plus3 x y z = x + y + z

let map3_test1 () = assert (List_utils.map3 plus3 [] [] [] = [])
let map3_test2 () = assert (List_utils.map3 plus3 [1; 2] [0; 1] [3; 0] = [4; 3])

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
    "split3", [
      test_case "Empty" `Quick split3_test1;
      test_case "One element" `Quick split3_test2;
      test_case "Two elements" `Quick split3_test3;
    ];
    "map3", [
      test_case "Test" `Quick map3_test1;
      test_case "Test" `Quick map3_test2;
    ]
  ]
