let split = List.partition SSL.is_pure

let rec apply phi = match phi with
  | SSL.And (psi1, psi2) -> SSL.mk_and [apply psi1; apply psi2]
  | SSL.Or (psi1, psi2) -> SSL.mk_or [apply psi1; apply psi2]
  | SSL.Star psis ->
    let pure, spatial = split psis in
    SSL.mk_and [SSL.mk_and pure; SSL.mk_star (List.map apply spatial)]
  | SSL.Septraction (psi1, psi2) -> SSL.mk_septraction (apply psi1) (apply psi2)
  | SSL.GuardedNeg (psi1, psi2) -> SSL.mk_gneg (apply psi1) (apply psi2)
  | SSL.Not psi -> SSL.mk_not (apply psi)
  | SSL.Exists (xs, psi) -> SSL.mk_exists xs (apply psi)
  | SSL.Forall (xs, psi) -> SSL.mk_forall xs (apply psi)

  | Eq _ | Distinct _ | Pure _ -> SSL.mk_and [phi; SSL.mk_emp ()]
  | PointsTo _ | LS _ | DLS _ -> phi
