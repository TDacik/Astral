module SL = SL_testable
open SL

let apply = RemoveVariadic.apply

let equal () =
  let phi = SL.mk_eq [w; x; y; z] in
  let expected = (w == x) && (x == y && (y == z)) in
  SL.check_equal (apply phi) expected

let distinct () =
  let phi = SL.mk_distinct [x; y; z] in
  let expected = (x != y) && (x != z && (y != z)) in
  SL.check_equal (apply phi) expected

(*
let exists () =
  let phi = SL.mk_exists [x; y] (x == y) in
  let expected = SL.mk_exists [x] @@ SL.mk_exists [y] (x == y) in
  SL.check_equal (apply phi) expected
*)

let () =
  run "RemoveVariadic" [
    "apply", [
      test_case "equal" `Quick equal;
      test_case "distinct" `Quick distinct;
      (*test_case "exists" `Quick exists; *)
    ]
  ]
