(* List utilities
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let all_equal eq = function
  | [] -> true
  | x :: xs -> List.for_all (eq x) xs

(* TODO: fix quadratic complexity *)
let rec all_distinct eq = function
  | [] -> true
  | x :: xs -> List.for_all (fun y -> not @@ eq x y) xs && all_distinct eq xs

let diagonal_product xs =
  BatList.fold_lefti (fun acc i x ->
    BatList.fold_lefti (fun acc j y ->
      if i < j then (x, y) :: acc
      else acc
    ) acc xs
  ) [] xs
