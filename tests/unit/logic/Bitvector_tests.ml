(* Tests for bitvector representation.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

let bv = Alcotest.pair Alcotest.int Alcotest.int

let check ~actual expected =
  Alcotest.check' bv ~msg:"" ~actual ~expected

let check_bool ~actual expected =
  Alcotest.check' Alcotest.bool ~msg:"" ~actual ~expected

let check_str ~actual expected =
  Alcotest.check' Alcotest.string ~msg:"" ~actual ~expected

let check_list ~actual expected =
  Alcotest.check' (Alcotest.list bv) ~msg:"" ~actual ~expected

(** Constructors *)

let full_zeros_test1 () =
  let actual = Bitvector.full_zeros 1 in
  check ~actual (0, 1)

let full_zeros_test2 () =
  let actual = Bitvector.full_zeros 4 in
  check ~actual (0, 4)

let full_ones_test1 () =
  let actual = Bitvector.full_ones 1 in
  check ~actual (1, 1)

let full_ones_test2 () =
  let actual = Bitvector.full_ones 4 in
  check ~actual (15, 4)

(** Access *)

let nth_test1 () =
  let actual = Bitvector.nth (0, 1) 0 in
  check_bool ~actual false

let nth_test2 () =
  let actual = Bitvector.nth (1, 1) 0 in
  check_bool ~actual true

let nth_test3 () =
  let actual = Bitvector.nth (2, 2) 0 in
  check_bool ~actual false

let nth_test4 () =
  let actual = Bitvector.nth (2, 2) 1 in
  check_bool ~actual true

(** To string *)

let to_string_test1 () =
  let actual = Bitvector.to_string (1, 2) in
  check_str ~actual "#b01"

let to_string_test2 () =
  let actual = Bitvector.to_string (3, 2) in
  check_str ~actual "#b11"

let of_string_bin_test1 () =
  let actual = Bitvector.of_string "#b000" in
  check ~actual (0, 3)

let of_string_bin_test2 () =
  let actual = Bitvector.of_string "#b01" in
  check ~actual (1, 2)

let of_string_hex_test1 () =
  let actual = Bitvector.of_string "#x000" in
  check ~actual (0, 12)

let of_string_hex_test2 () =
  let actual = Bitvector.of_string "#x2a" in
  check ~actual (42, 8)

(** To set *)

let to_set_test1 () =
  let actual = Bitvector.to_set (0, 3) in
  check_list ~actual []

let to_set_test2 () =
  let actual = Bitvector.to_set (3, 2) in
  check_list ~actual [(0, 2); (1, 2)]

let to_set_test3 () =
  let actual = Bitvector.to_set (5, 4) in
  check_list ~actual [(0, 4); (2, 4)]

let () =
  run "Bitvectors" [
    "Constructors", [
      test_case "full_zeros 1 = (0, 1)"  `Quick full_zeros_test1;
      test_case "full_zeros 1 = (0, 1)"  `Quick full_zeros_test2;
      test_case "full_ones 4  = (1, 1)"  `Quick full_ones_test1;
      test_case "full_ones 4  = (15, 4)" `Quick full_ones_test2;
    ];
    "nth", [
      test_case "nth (0, 1) 0"  `Quick nth_test1;
      test_case "nth (1, 1) 0"  `Quick nth_test2;
      test_case "nth (2, 2) 0"  `Quick nth_test3;
      test_case "nth (2, 2) 1"  `Quick nth_test4;
    ];
    "To string", [
      test_case "to_string (1, 2)"  `Quick to_string_test1;
      test_case "to_string (3, 2)"  `Quick to_string_test2;
      test_case "of_string (#b000)" `Quick of_string_bin_test1;
      test_case "of_string (#b01)"  `Quick of_string_bin_test2;
      test_case "of_string (#x000)" `Quick of_string_hex_test1;
      test_case "of_string (#x2a)"  `Quick of_string_hex_test2;
    ];
    "To set", [
      test_case "to_set (0, 3)"  `Quick to_set_test1;
      test_case "to_set (3, 2)"  `Quick to_set_test2;
      test_case "to_set (5, 4)"  `Quick to_set_test3;
    ];
  ]
