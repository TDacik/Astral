(* Tests
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Infix

let x = SSL.mk_var "x" Sort.loc_ls
let y = SSL.mk_var "y" Sort.loc_ls
let z = SSL.mk_var "z" Sort.loc_ls
let nil = SSL.mk_nil ()

(** Precise -> imprecise *)

let to_imprecise = PreciseToImprecise.to_imprecise

let to_imprecise_test1 () =
  let phi = x |-> y in
  assert (to_imprecise phi === phi)

let to_imprecise_test2 () =
  let phi = x == y in
  let phi' = (x == y) && (SSL.mk_emp ()) in
  assert (to_imprecise phi === phi')

let to_imprecise_test3 () =
  let phi = (x == y) * (x |-> z) in
  let phi' = (x == y) && (x |-> z) in
  assert (to_imprecise phi === phi')

let to_imprecise_test4 () =
  let phi = SSL.mk_star [x |-> y; y |-> x; x == y; y == z] in
  let phi' = ((x == y) && (y == z)) && ((x |-> y) * (y |-> x)) in
  assert (to_imprecise phi === phi')


(** Imprecise -> precise *)

let to_precise phi =
  PreciseToImprecise.to_precise phi
  |> Simplifier.simplify

let to_precise_test1 () =
  let phi = x |-> y in
  assert (to_precise phi === phi)

let to_precise_test2 () =
  let phi = x == y in
  let phi' = (x == y) * (SSL.mk_true ()) in
  assert (to_precise phi === phi')

let to_precise_test3 () =
  let pure = SSL.mk_and [x == y; y == nil] in
  let spatial = SSL.mk_emp () in
  let phi = SSL.mk_and [pure; spatial] in
  let phi' = SSL.mk_star [x == y; y == nil] in
  assert (to_precise phi === phi')

let to_precise_test4 () =
  let spatial = SSL.mk_star [x |-> y; y |-> x] in
  let pure = SSL.mk_and [x == y; y == nil] in
  let phi = SSL.mk_and [spatial; pure] in
  let phi' = SSL.mk_star [x |-> y; y |-> x; x == y; y == nil] in
  assert (to_precise phi === phi')


(** Removing of variadic operators *)

let apply = RemoveVariadic.apply

let remove_variadic_eq_test1 () =
  let phi = SSL.mk_eq_list [x; y] in
  assert (apply phi === phi)

let remove_variadic_eq_test2 () =
  let phi = SSL.mk_eq_list [x; y; z] in
  let phi' = (x == y) && (y == z) in
  assert (apply phi === phi')

let remove_variadic_distinct_test1 () =
  let phi = SSL.mk_distinct_list [x; y] in
  assert (apply phi === phi)

let remove_variadic_distinct_test2 () =
  let phi = SSL.mk_distinct_list [x; y; z] in
  let phi' = ((y != z) && (x != z)) && (x != y) in
  assert (apply phi === phi')

let remove_variadic_star_test1 () =
  let phi = (x |-> y) * (y |-> x) in
  assert (apply phi === phi)

let remove_variadic_star_test2 () =
  let phi = SSL.mk_star [x |-> x; x |-> x; x |-> x] in
  let phi' = SSL.mk_star [x |-> x; SSL.mk_star [x |-> x; x |-> x]] in
  assert (apply phi === phi')

(** Pure terms *)
let apply = PurePreprocessing.apply

let pure_test1 () =
  let b = SMT.Boolean.mk_var "b" in
  let phi = SSL.mk_and [SSL.mk_pure b; SSL.mk_pure b] in
  let phi' = SSL.mk_pure @@ SMT.Boolean.mk_and [b; b] in
  assert (apply phi === phi')

let pure_test2 () =
  let phi = SSL.mk_not @@ SSL.mk_pure @@ SMT.Boolean.mk_true () in
  let phi' = SSL.mk_pure @@ SMT.Boolean.mk_false () in
  assert (apply phi === phi')

let () =
  run "Preprocessors" [
    "Precise -> Imprecise", [
      test_case "Test" `Quick to_imprecise_test1;
      test_case "Test" `Quick to_imprecise_test2;
      test_case "Test" `Quick to_imprecise_test3;
      test_case "Test" `Quick to_imprecise_test4;
    ];
    "Imprecise -> Precise", [
      test_case "Test" `Quick to_precise_test1;
      test_case "Test" `Quick to_precise_test2;
      test_case "Test" `Quick to_precise_test3;
      test_case "Test" `Quick to_precise_test4;
    ];
    "Removing of variadic operators", [
      test_case "Test" `Quick remove_variadic_eq_test1;
      test_case "Test" `Quick remove_variadic_eq_test2;
      test_case "Test" `Quick remove_variadic_distinct_test1;
      test_case "Test" `Quick remove_variadic_distinct_test2;
      test_case "Test" `Quick remove_variadic_star_test1;
      test_case "Test" `Quick remove_variadic_star_test2;
    ];
    "TODO: Pure preprocessing", [
      test_case "Test" `Quick pure_test1;
      test_case "Test" `Quick pure_test2;
    ];
  ]
