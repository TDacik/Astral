(* Tests for Astral's internal SMT representation
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SMT

(** Auxiliary functions *)

let (==) = SMT.equal
let (!=) x y = not @@ SMT.equal x y

(** Test fixures *)

let sort = Sort.Int
let set_sort = Sort.Set sort

let x = Arithmetic.mk_var "x"
let y = Arithmetic.mk_var "y"
let z = Arithmetic.mk_var "z"

let c1 = Arithmetic.mk_const 1
let c2 = Arithmetic.mk_const 2
let c3 = Arithmetic.mk_const 3

(** Tests *)

let map_vars1 () = assert (map_vars (fun x sort -> Variable (x, sort)) True == True)

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
