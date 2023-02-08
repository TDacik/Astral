(* Tests
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Alcotest
open Astral_lib

open SSL.Infix

let x = SSL.mk_var "x"
let y = SSL.mk_var "y"
let z = SSL.mk_var "z"

let (===) = SSL.equal
let preprocess = PreciseToImprecise.preprocess

let prec_sh1 () =
  let phi = (x == y) * (x |-> z) in
  let phi' = (x == y) && (x |-> z) in
  assert (preprocess phi === phi')

let () =
  run "Preprocessors" [
    "PreciseToImprecise", [
      test_case "Test" `Quick prec_sh1;
    ];
  ]
