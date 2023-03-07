(* Tests
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL.Infix

let x = SSL.mk_var "x"
let y = SSL.mk_var "y"
let z = SSL.mk_var "z"

let (===) = SSL.equal

(** Precise -> imprecise *)

let apply = PreciseToImprecise.apply

let prec_imprecise_test1 () =
  let phi = x == y in
  let phi' = (x == y) && (SSL.mk_emp ()) in
  assert (apply phi === phi')

let prec_imprecise_test2 () =
  let phi = (x == y) * (x |-> z) in
  let phi' = (x == y) && (x |-> z) in
  assert (apply phi === phi')

let prec_imprecise_test3 () =
  let phi = SSL.mk_star [x |-> y; y |-> x; x == y; y == z] in
  let phi' = ((x == y) && (y == z)) && ((x |-> y) * (y |-> x)) in
  assert (apply phi === phi')

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
    "PreciseToImprecise", [
      test_case "Test" `Quick prec_imprecise_test1;
      test_case "Test" `Quick prec_imprecise_test2;
      test_case "Test" `Quick prec_imprecise_test3;
    ];
    "Removing of variadic operators", [
      test_case "Test" `Quick remove_variadic_eq_test1;
      test_case "Test" `Quick remove_variadic_eq_test2;
      test_case "Test" `Quick remove_variadic_distinct_test1;
      test_case "Test" `Quick remove_variadic_distinct_test2;
      test_case "Test" `Quick remove_variadic_star_test1;
      test_case "Test" `Quick remove_variadic_star_test2;
    ];
    "Pure preprocessing", [
      test_case "Test" `Quick pure_test1;
      test_case "Test" `Quick pure_test2;
    ];
  ]
