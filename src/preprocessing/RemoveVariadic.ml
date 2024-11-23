(* Replace variadic operators by their binary versions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let rec aux combine fn = function
  | [x1; x2] -> fn [x1; x2]
  | x1 :: x2 :: xs -> combine [fn [x1; x2]; aux combine fn (x2 :: xs)]

let rec apply phi =
  SL.map_view (fun psi -> match psi with
    | SL.Eq xs -> aux SL.mk_and SL.mk_eq xs

    | SL.Distinct [x1; x2] -> SL.mk_distinct [x1; x2]
    | SL.Distinct xs ->
      List_utils.diagonal_product xs
      |> List.map (fun (x, y) -> SL.mk_distinct [x; y])
      |> SL.mk_and

    | SL.And psis -> aux SL.mk_and SL.mk_and psis
    | SL.Or psis -> aux SL.mk_or SL.mk_or psis
    | SL.Star psis -> aux SL.mk_star SL.mk_star psis

    | SL.Exists ([x], psi) -> SL.mk_exists [x] psi
    | SL.Forall ([x], psi) -> SL.mk_forall [x] psi
    | SL.Exists (x :: xs, psi) -> SL.mk_exists [x] (apply @@ SL.mk_exists xs psi)
    | SL.Forall (x :: xs, psi) -> SL.mk_forall [x] (apply @@ SL.mk_exists xs psi)
  ) phi
