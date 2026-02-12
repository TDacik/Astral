module L = BaseLogic_testable
open L

module SL = L.SeparationLogic

let star_neutral () =
  let actual = SL.mk_star [SL.emp; SL.mk_pto x y] in
  let expected = SL.mk_pto x y in
  L.check_equal actual expected

let exists_reorder1 () =
  let phi1 = Quantifiers.mk_exists [Var.x; Var.y] (SL.mk_pto x y) in
  let phi2 = Quantifiers.mk_exists [Var.y; Var.x] (SL.mk_pto y x) in
  let actual = (===) phi1 phi2 in
  Alcotest.check' Alcotest.bool ~msg:"" ~actual ~expected:true

(* TODO: is this needed?
let exists_reorder2 () =
  let phi1 = Quantifiers.mk_exists [Var.x; Var.y] (SL.mk_pto x y) in
  let phi2 = Quantifiers.mk_exists [Var.y; Var.z] (SL.mk_pto z y) in
  let actual = (===) phi1 phi2 in
  Alcotest.check' Alcotest.bool ~msg:"" ~actual ~expected:true
  *)


let () =
  run "BaseLogic" [
    "Smart constructors", [
      test_case "Star neutral" `Quick star_neutral;
    ];
    "Alpha-equivalence", [
      test_case "Existential reorder" `Quick exists_reorder1;
      (*test_case "Existential reorder" `Quick exists_reorder2;*)
    ];
  ]
