(* Tests for Astral's internal SMT representation
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

(** Auxiliary functions *)

let (==) = SMT.equal
let (!=) x y = not @@ SMT.equal x y

module SMT = SMT_testable
open SMT

let sort = Sort.mk_set Sort.int

let may_disjoint_test1 () =
  let set1 = SMT.Sets.mk_constant sort [x] in
  let set2 = SMT.Sets.mk_constant sort [x] in
  SMT.check_not_list SMT.Sets.may_disjoint [set1; set2]

let may_disjoint_test2 () =
  let set1 = SMT.Sets.mk_constant sort [x] in
  let set2 = SMT.Sets.mk_constant sort [y] in
  SMT.check_list SMT.Sets.may_disjoint [set1; set2]

let may_disjoint_test3 () =
  let set1 = SMT.mk_var "X" sort in
  let set2 = SMT.Sets.mk_constant sort [x] in
  SMT.check_list SMT.Sets.may_disjoint [set1; set2]

let () =
  run "SMT" [
    "Sets.may_disjoint", [
      test_case "{x} # {x}"  `Quick may_disjoint_test1;
      test_case "{x} # {y}"  `Quick may_disjoint_test2;
      test_case "X # {x}"    `Quick may_disjoint_test3;
    ];
  ]

(** Tests

let map_vars1 () = assert (map_vars (fun x sort -> Variable (Identifier.show x, sort)) True == True)

let map_vars2 () =
  assert (
    map_vars (fun x sort -> Variable (x ^ x, sort))
              (Variable ("x", Sort.Int)) == (Variable ("xx", Sort.Int))
  )

let map () =
  assert (
    let fn = (fun t -> match t with Forall (_, _, phi) -> phi | _ -> t) in
    map fn (Quantifier.mk_forall [x] x) == x
  )

let substitute1 () =
  assert (substitute x x y == y)

let substitute2 () =
  assert (substitute (Quantifier.mk_forall [x] x) x y == (Quantifier.mk_forall [x] x))

let substitute3 () =
  assert (substitute (Quantifier.mk_forall [x] y) y z == (Quantifier.mk_forall [x] z))

let size_var () =
  assert (size x = 1)

let size_term () =
  assert (size (Arithmetic.mk_plus c1 c2) = 3)

let size_quantifier () =
  assert (size (Quantifier.mk_forall [x; y] (Arithmetic.mk_lesser_eq x y)) = 5)

let and_smart_cons1 () = assert (Boolean.mk_and [] == True)
let and_smart_cons2 () = assert (Boolean.mk_and [True; True] == True)
let and_smart_cons3 () = assert (Boolean.mk_and [True; False] == False)

let set_smart_cons1 () =
  assert (
    Set.mk_union [(Set.mk_enumeration sort [c1; c2]); (Set.mk_singleton c3)] set_sort
    == (Set.mk_enumeration set_sort [c1; c2; c3])
  )

let () =
  run "SMT" [
    "map_vars", [
      test_case "Test"  `Quick map_vars1;
      test_case "Test"  `Quick map_vars2;
    ];
    "map", [
      test_case "Test"  `Quick map;
    ];
    "substitute", [
      test_case "Test"  `Quick substitute1;
      test_case "Test"  `Quick substitute2;
      test_case "Test"  `Quick substitute3;
    ];
    "size", [
      test_case "Test"  `Quick size_var;
      test_case "Test"  `Quick size_term;
      test_case "Test"  `Quick size_quantifier;
    ];
    "and smart constructor", [
      test_case "Test"  `Quick and_smart_cons1;
      test_case "Test"  `Quick and_smart_cons2;
      test_case "Test"  `Quick and_smart_cons3;
    ];
    "set smart constructor", [
      test_case "Test"  `Quick set_smart_cons1;
    ];
  ]

*)
