(* Conversion between precise and imprecise semantics of (dis-)equalities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL

(** Precise -> Imprecise *)

let to_imprecise_sh psis =
  let pure, spatial = List.partition SSL.is_pure psis in
  match pure, spatial with
  | [], spatial -> SSL.mk_star spatial
  | _ -> SSL.mk_and [SSL.mk_and pure; SSL.mk_star spatial]

let to_imprecise_arbitrary phi =
  SSL.map (function
    | Var x -> Var x
    | psi when SSL.is_pure psi -> SSL.mk_and [psi; SSL.mk_emp ()]
    | psi -> psi
  ) phi

let to_imprecise phi = match SSL.as_query phi with
  | SymbolicHeap_SAT psis -> to_imprecise_sh psis
  | SymbolicHeap_ENTL (lhs, rhs) -> SSL.mk_gneg (to_imprecise_sh lhs) (to_imprecise_sh rhs)
  | _ -> to_imprecise_arbitrary phi

(** Imprecise -> Precise *)

let to_precise_sh phi =
  match phi with
  | Eq _ | Distinct _ -> SSL.mk_star [phi; SSL.mk_true ()]
  | phi ->
    SSL.map
      (fun psi -> match psi with
        | Var x -> Var x
        | And (psi1, psi2) -> SSL.mk_star [psi1; psi2]
        | psi -> psi
      ) phi

let to_precise_arbitrary phi =
  SSL.map (function
    | Var x -> Var x
    | psi when SSL.is_pure psi -> SSL.mk_star [psi; SSL.mk_true ()]
    | psi -> psi
 ) phi

let rec is_pure_part = function
  | Eq _ | Distinct _ -> true
  | And (lhs, rhs) -> is_pure_part lhs && is_pure_part rhs
  | _ -> false

let rec is_spatial_part = function
  | PointsTo _ | LS _ | DLS _ | NLS _ | Emp -> true
  | Star psis -> List.for_all is_spatial_part psis
  | _ -> false

let is_imprecise_sh = function
  | Eq _ | Distinct _ | PointsTo _ | LS _ | DLS _ | NLS _ | Emp -> true
  | And (lhs, rhs) ->
    (is_pure_part lhs && is_spatial_part rhs)
    || (is_spatial_part lhs && is_pure_part rhs)
  | phi -> is_spatial_part phi

(* Translate formulae from SL with imprecise equalities. *)
let to_precise phi =
  (match phi with
  | phi when is_imprecise_sh phi -> to_precise_sh phi
  | GuardedNeg (lhs, rhs) when is_imprecise_sh lhs && is_imprecise_sh rhs ->
    SSL.mk_gneg (to_precise_sh lhs) (to_precise_sh rhs)
  | GuardedNeg (lhs, Exists (xs, rhs)) when is_imprecise_sh lhs && is_imprecise_sh rhs ->
    SSL.mk_gneg (to_precise_sh lhs) (SSL.mk_exists xs @@ to_precise_sh rhs)
  | _ -> to_precise_arbitrary phi)
  |> Simplifier.fold_stars
