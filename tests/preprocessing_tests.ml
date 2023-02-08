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

let prec_sh1 () =
  let phi = (x == y) * (x |-> z) in
  let phi' = (x == y) && (x |-> z) in
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

(** Pure terms *)
let apply = PurePreprocessing.apply

let pure_test1 () =
  let b = SMT.Boolean.mk_var "b" in
  let phi = SSL.mk_and [SSL.mk_pure b; SSL.mk_pure b] in
  let phi' = SSL.mk_pure @@ SMT.Boolean.mk_and [b; b] in
  assert (apply phi === phi')


let () =
  run "Preprocessors" [
    "PreciseToImprecise", [
      test_case "Test" `Quick prec_sh1;
    ];
    "Removing of variadic operators", [
      test_case "Test" `Quick remove_variadic_eq_test1;
      test_case "Test" `Quick remove_variadic_eq_test2;
      test_case "Test" `Quick remove_variadic_distinct_test1;
      test_case "Test" `Quick remove_variadic_distinct_test2;
    ];
    "Pure preprocessing", [
      test_case "Test" `Quick pure_test1;
    ];
  ]
