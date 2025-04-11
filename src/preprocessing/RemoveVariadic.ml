(* Replace variadic operators by their binary versions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let rec process_eq = function
  | [x1; x2] -> SL.mk_eq [x1; x2]
  | x1 :: x2 :: xs -> SL.mk_and [SL.mk_eq [x1; x2]; process_eq (x2 :: xs)]

let process_distinct = function
  | [x1; x2] -> SL.mk_distinct [x1; x2]
  | xs ->
    List_utils.diagonal_product xs
    |> List.map (fun (x, y) -> SL.mk_distinct [x; y])
    |> SL.mk_and

let rec process_variadic fn = function
  | [x1; x2] -> fn [x1; x2]
  | x1 :: x2 :: xs -> fn [fn [x1; x2]; process_variadic fn xs]

let rec apply phi =
  SL.map_view (fun psi -> match psi with
    | SL.Eq xs -> process_eq xs
    | SL.Distinct xs -> process_distinct xs
    (*
      SL.mk_distinct [x1; x2]
    | SL.Distinct xs ->
      List_utils.diagonal_product xs
      |> List.map (fun (x, y) -> SL.mk_distinct [x; y])
      |> SL.mk_and
    *)
    | SL.And psis -> process_variadic SL.mk_and psis
    | SL.Or psis -> process_variadic SL.mk_or psis
    | SL.Star psis -> process_variadic SL.mk_star psis

    | SL.Exists ([x], psi) -> SL.mk_exists [x] psi
    | SL.Forall ([x], psi) -> SL.mk_forall [x] psi
    | SL.Exists (x :: xs, psi) -> SL.mk_exists [x] (apply @@ SL.mk_exists xs psi)
    | SL.Forall (x :: xs, psi) -> SL.mk_forall [x] (apply @@ SL.mk_exists xs psi)
  ) phi
