(* Tests for Astral's internal SMT representation
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SMT

(** Auxiliary functions *)

let (===) = SMT.equal
let (!==) x y = not @@ SMT.equal x y

(** Test fixures *)

let sort = Sort.Int
let set_sort = Sort.Set sort

let x = Set.mk_var "x" set_sort
let y = Set.mk_var "y" set_sort
let z = Set.mk_var "z" set_sort

(** Tests *)

let simplify_disjoint_test1 () =
  let phi = Set.mk_disjoint_list [x; y] in
  assert (Backend_preprocessor.apply phi === phi)

let simplify_disjoint_test2 () =
  let phi = Set.mk_disjoint_list [x; y; z] in
  let phi' =
    Boolean.mk_and [
      Set.mk_disjoint x y;
      Set.mk_disjoint (Set.mk_union [x; y] (SMT.get_sort x)) z;
    ]
  in
  assert (Backend_preprocessor.apply phi === phi')

let () =
  run "SMT" [
    "simplify_disjoint", [
      test_case "Test"  `Quick simplify_disjoint_test1;
      test_case "Test"  `Quick simplify_disjoint_test2;
    ];
  ]
