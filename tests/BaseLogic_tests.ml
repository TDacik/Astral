module L = BaseLogic_testable
open L

module SL = L.SeparationLogic

let star_neutral () =
  let actual = SL.mk_star [SL.emp; SL.mk_pto x y] in
  let expected = SL.mk_pto x y in
  L.check_equal actual expected

let () =
  run "BaseLogic" [
    "Smart constructors", [
      test_case "Star neutral" `Quick star_neutral;
    ];
  ]
