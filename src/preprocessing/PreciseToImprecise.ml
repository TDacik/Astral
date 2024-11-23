(* Conversion between precise and imprecise semantics of (dis-)equalities.
 *
 * Symbolic heaps:
 *   - to imprecise : * (atoms) <~> ( *spatial) /\ (/\ pure)
 *
 * General formulae:
 *
 *   - to imprecise : pure(X) ~> pure(X) /\ emp
 *   - to precise :   pure(X) ~> pure(X) * true
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SL

(** Precise -> Imprecise *)

let to_imprecise_sh psis =
  let pure, spatial = List.partition SL.is_pure psis in
  match pure, spatial with
  | [], spatial -> SL.mk_star spatial
  | _ -> SL.mk_and [SL.mk_and pure; SL.mk_star spatial]

let to_imprecise_arbitrary phi =
  SL.map (function
    | psi when SL.is_pure psi -> SL.mk_and [psi; SL.emp]
  ) phi

let to_imprecise phi = match SL.as_query phi with
  | SymbolicHeap_SAT psis -> to_imprecise_sh psis
  | SymbolicHeap_ENTL (lhs, rhs) -> SL.mk_gneg (to_imprecise_sh lhs) (to_imprecise_sh rhs)
  | _ -> to_imprecise_arbitrary phi

(** Imprecise -> Precise *)

let to_precise_sh phi = match SL.view phi with
  | Eq _ | Distinct _ | Pure _ -> SL.mk_star [phi; SL.tt]
  | _ ->
    SL.map_view
      (fun psi -> match psi with
        | And psis -> SL.mk_star psis
      ) phi

let to_precise_arbitrary phi =
  SL.map (function
    | psi when SL.is_pure psi -> SL.mk_star [psi; SL.tt]
    | other -> other
 ) phi

let rec is_pure_part phi = match SL.view phi with
  | Eq _ | Distinct _ | Pure _ -> true
  | And psis -> List.for_all is_pure_part psis
  | _ -> false

let rec is_spatial_part phi = match SL.view phi with
  | PointsTo _ | Predicate _ | Emp -> true
  | Star psis -> List.for_all is_spatial_part psis
  | _ -> false

(** Imprecise symbolic heap is in of the following forms:
    /\ (pure1 ... pure n, emp)
    /\ (pure1 ... pure n, * (...)) *)

let is_imprecise_sh phi = match SL.view phi with
  | Eq _ | Distinct _ | PointsTo _ | Predicate _ | Emp -> true
  | And psis ->
    let _, spatial = List.partition SL.is_pure psis in
    begin match List.map SL.view spatial with
    | [] -> true
    | [_] when SL.is_atom @@ List.hd spatial -> true
    | [Star atoms] -> List.for_all SL.is_atom atoms
    | _ -> false
    end

  | _ -> is_spatial_part phi

(** TODO: existential symbolic heaps *)
let to_precise phi = match SL.view phi with
  | _ when is_imprecise_sh phi -> to_precise_sh phi
  | GuardedNeg (lhs, rhs) when is_imprecise_sh lhs && is_imprecise_sh rhs ->
    SL.mk_gneg (to_precise_sh lhs) (to_precise_sh rhs)
  (*
  | GuardedNeg (lhs, Exists (xs, rhs)) when is_imprecise_sh lhs && is_imprecise_sh rhs ->
    SL.mk_gneg (to_precise_sh lhs) (SL.mk_exists xs @@ to_precise_sh rhs)
  *)
  | _ -> to_precise_arbitrary phi
