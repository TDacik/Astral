module SL = SL_testable
open SL

let check = SL.check_apply RemoveVariadic.apply

let equal () =
  let input = SL.mk_eq [w; x; y; z] in
  let expected = (w == x) && (x == y && (y == z)) in
  check ~input ~expected

let distinct () =
  let input = SL.mk_distinct [x; y; z] in
  let expected = (x != y) && (x != z && (y != z)) in
  check ~input ~expected

let star_test1 () =
  let input = SL.mk_star [x |-> nil; y |-> nil; z |-> nil] in
  let expected = (x |-> nil) * ((y |-> nil) * (z |-> nil)) in
  check ~input ~expected

(*let exists () =
 * let phi = SL.mk_exists [x; y] (x == y) in
 * let expected = SL.mk_exists [x] @@ SL.mk_exists [y] (x == y) in
 * SL.check_equal (apply phi) expected
 *)

let () =
  run "RemoveVariadic" [
    "apply", [
      test_case "equal" `Quick equal;
      test_case "distinct" `Quick distinct;
      test_case "star" `Quick star_test1;
      (*test_case "exists" `Quick exists; *)
    ]
  ]
