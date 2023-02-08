(* Tests for operations over SSL formulae
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Infix

let x = SSL.mk_pure (SMT.Arithmetic.mk_var "x")
let y = SSL.mk_pure (SMT.Arithmetic.mk_var "y")

let is_precise () =
  assert (is_pure (x == y));
  assert (is_pure (x != y))

let () =
  run "SSL" [
    "is_precise", [
      test_case "Test"  `Quick is_precise;
    ];
  ]
