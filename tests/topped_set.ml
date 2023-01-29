open Alcotest

open Astral_lib

(** Auxiliary functions *)

let (==) set1 set2 =
  let sort = List.sort Stdlib.compare in
  match set1, set2 with
  | None, None -> true
  | Some xs1, Some xs2 -> List.equal Stdlib.(=) (sort xs1) (sort xs2)

(** Test fixtures *)

module Base = Set.Make(Int)
module Set = Topped_set.Lift(Base)

let top = Set.top

let a = Set.of_list [1]
let b = Set.of_list [1; 2]
let c = Set.of_list [2; 3]

(** Tests *)

let empty () = assert (Set.is_empty Set.empty)


let add_lifted () = assert (Set.equal (Set.add 2 a) b)
let add_top () = assert (Set.equal (Set.add 2 top) top)


let remove_lifted () = assert (Set.equal (Set.remove 2 b) a)
let remove_top () = assert (Set.equal (Set.remove 2 top) top)


let product_empty1 () = assert (Some [] = (Set.cartesian_product Set.empty a))
let product_empty2 () = assert (Some [] = (Set.cartesian_product a Set.empty))
let product () =
  let (Some xs) = Set.cartesian_product b c in
  List.iter (fun (x, y) -> Format.printf "(%d, %d)\n" x y) xs;
  assert (Some [(1,2); (1,3); (2,2); (2,3)] == (Set.cartesian_product b c))


let () =
  run "Topped set" [
    "empty", [
      test_case "Empty" `Quick empty;
    ];
    "add", [
      test_case "Add to precise set"  `Quick add_lifted;
      test_case "Add to top set"      `Quick add_top;
    ];
    "remove", [
      test_case "Remove from precise set" `Quick remove_lifted;
      test_case "Remove from top set"     `Quick remove_top;
    ];
    "product", [
      test_case "Product lhs empty"         `Quick product_empty1;
      test_case "Product rhs empty"         `Quick product_empty2;
      test_case "Product of non-empty sets" `Quick product;
    ];
  ]
