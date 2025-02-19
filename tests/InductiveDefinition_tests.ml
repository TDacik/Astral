(* Tests for operations over inductive definitions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module SL = SL_testable
open SL

let ls_def x y =
  SL.mk_or [
    SL.mk_eq [x; y];
    SL.mk_exists' [SL_builtins.loc_ls] (fun [n] ->
      SL.mk_star [
        SL.mk_distinct [x; y];
        SL.mk_pto x y;
        SL.mk_predicate "ls" [n; x]
  ])]

let ls = InductiveDefinition.mk "ls" [Var.x; Var.y] @@ ls_def x y

let instantiate_test1 () =
  let actual = InductiveDefinition.instantiate ~refresh:false ls [u; v] in
  let expected = ls_def u v in
  SL.check_equal actual expected

let instantiate_test2 () =
  let actual = InductiveDefinition.instantiate ~refresh:true ls [u; v] in
  let expected = ls_def u v in
  SL.check_equal actual expected

let () =
  run "Inductive definitions" [
    "instantiate", [
      test_case "no refresh" `Quick instantiate_test1;
      test_case "refresh"    `Quick instantiate_test2;
    ];
  ]

