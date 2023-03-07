(* Replace variadic operators by their binary versions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let rec apply  phi = match phi with
  | SSL.Eq [x1; x2] -> SSL.Eq [x1; x2]
  | SSL.Eq (x1 :: x2 :: xs) -> SSL.mk_and [SSL.Eq [x1; x2]; apply (SSL.Eq (x2 :: xs))]

  | SSL.Distinct [x1; x2] -> SSL.Distinct [x1; x2]
  | SSL.Distinct xs ->
      List_utils.diagonal_product xs
      |> List.map (fun (x, y) -> SSL.mk_distinct x y)
      |> SSL.mk_and

  | SSL.And (psi1, psi2) -> SSL.mk_and [apply psi1; apply psi2]
  | SSL.Or (psi1, psi2) -> SSL.mk_or [apply psi1; apply psi2]
  | SSL.Star [psi1; psi2] -> SSL.mk_star [psi1; psi2]
  | SSL.Star (psi :: psis) -> SSL.mk_star [psi; apply @@ SSL.mk_star psis]
  | SSL.Septraction (psi1, psi2) -> SSL.mk_septraction (apply psi1) (apply psi2)
  | SSL.GuardedNeg (psi1, psi2) -> SSL.mk_gneg (apply psi1) (apply psi2)
  | SSL.Not psi -> SSL.mk_not (apply psi)
  | SSL.Exists ([x], psi) -> SSL.mk_exists [x] (apply psi)
  | SSL.Forall ([x], psi) -> SSL.mk_forall [x] (apply psi)
  | SSL.Exists (x :: xs, psi) -> SSL.mk_exists [x] (apply @@ SSL.mk_exists xs psi)
  | SSL.Forall (x :: xs, psi) -> SSL.mk_forall [x] (apply @@ SSL.mk_exists xs psi)

  | Pure _ | PointsTo _ | LS _ | DLS _ -> phi
