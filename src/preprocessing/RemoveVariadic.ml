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

let rec process_variadic fn neutral = function
  | [] -> neutral
  | [x] -> x
  | [x1; x2] -> fn [x1; x2]
  | x1 :: x2 :: xs -> fn [fn [x1; x2]; process_variadic fn neutral xs]

let rec apply ?(symbolic_heap=false) phi =
  let res =
    SL.map_view (fun psi -> match psi with
      | SL.Eq xs -> `Modify (process_eq xs)
      | SL.Distinct xs -> `Modify (process_distinct xs)
      | SL.And psis -> `Modify (process_variadic SL.mk_and SL.tt psis)
      | SL.Or psis -> `Modify (process_variadic SL.mk_or SL.ff psis)
      | SL.Star psis -> `Modify (process_variadic SL.mk_star SL.emp psis)

      | SL.Exists ([x], psi) -> `Modify (SL.mk_exists [x] psi)
      | SL.Forall ([x], psi) -> `Modify (SL.mk_forall [x] psi)
      | SL.Exists (x :: xs, psi) -> `Modify (SL.mk_exists [x] (apply @@ SL.mk_exists xs psi))
      | SL.Forall (x :: xs, psi) -> `Modify (SL.mk_forall [x] (apply @@ SL.mk_exists xs psi))
      | _ -> `Skip
    ) phi
  in
  if not symbolic_heap then res
  else
    SL.map_view (fun psi -> match psi with
      | SL.And psis -> `Modify (SL.mk_star psis)
      | _ -> `Skip
    ) res
