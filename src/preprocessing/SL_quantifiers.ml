let rec apply locs phi =
  let apply = apply locs in
  match phi with
  | SSL.Var v -> SSL.Var v
  | SSL.Pure term -> SSL.mk_pure term
  | SSL.Eq xs -> SSL.mk_eq_list xs
  | SSL.Distinct xs -> SSL.mk_distinct_list xs
  | SSL.PointsTo (x, y) -> SSL.mk_pto_struct x y
  | SSL.LS (x, y) -> SSL.mk_ls x y
  | SSL.DLS (x, y, f, l) -> SSL.mk_dls x y f l
  | SSL.And (psi1, psi2) -> SSL.mk_and [apply psi1; apply psi2]
  | SSL.Or (psi1, psi2) -> SSL.mk_or [apply psi1; apply psi2]
  | SSL.Not psi -> SSL.mk_not (apply psi)
  | SSL.GuardedNeg (psi1, psi2) -> SSL.mk_gneg (apply psi1) (apply psi2)
  | SSL.Star psis -> SSL.mk_star @@ List.map apply psis
  | SSL.Septraction (psi1, psi2) -> SSL.mk_septraction (apply psi1) (apply psi2)
  | SSL.Exists (xs, psi) ->
      List.map (fun x ->
        List.map (fun l ->
          SSL.substitute psi x l
        ) locs
      ) xs
      |> List.concat
      |> SSL.mk_or
