let rec split phi = match phi with
  | SSL.Eq _ | SSL.Distinct _ | SSL.Pure _ -> ([], [phi], [])
  | SSL.PointsTo _ | SSL.LS _ | SSL.DLS _ -> ([], [], [phi])
  | SSL.Exists (xs, psi) ->
    let binders, pure, spatial = split psi in
    (xs @ binders, pure, spatial)

  | SSL.Star (psi1, psi2) ->
    let binders1, pure1, spatial1 = split psi1 in
    let binders2, pure2, spatial2 = split psi2 in
    (binders1 @ binders2, pure1 @ pure2, spatial1 @ spatial2)

let rec apply phi = match phi with
  | SSL.GuardedNeg (psi1, psi2) -> SSL.mk_gneg (apply psi1) (apply psi2)
  | _ ->
    let binders, pure, spatial = split phi in
    SSL.mk_exists binders (SSL.mk_and [SSL.mk_star spatial; SSL.mk_and pure])
