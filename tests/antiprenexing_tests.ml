(* Tests for antiprenexing
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Infix

let x = SSL.mk_var "x"
let y = SSL.mk_var "y"
let z = SSL.mk_var "z"

let apply = Antiprenexing.apply

(** Tests *)

let test1 () =
  let phi = SSL.mk_exists [x] @@ SSL.mk_star [x |-> x; x |-> x] in
  assert (apply phi === phi)

let test2 () =
  let phi = SSL.mk_exists [x] @@ SSL.mk_star [x |-> x; y |-> y] in
  let phi' = SSL.mk_star [y |-> y; SSL.mk_exists [x] (x |-> x)] in
  assert (apply phi === phi')

let () =
  run "Antiprenexing" [
    "apply", [
      test_case "Test" `Quick test1;
      test_case "Test" `Quick test2;
    ];
  ]
