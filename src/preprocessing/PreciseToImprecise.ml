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

module Logger = Logger.Make(struct let name = "Precise <-> Imprecise" let level = 3 end)

(** Precise -> Imprecise *)

let to_imprecise_sh psi =
  let pure, spatial = SL.as_symbolic_heap psi in
  match pure, spatial with
  | [], spatial -> SL.mk_star spatial
  | _ -> SL.mk_and [SL.mk_and pure; SL.mk_star spatial]

let to_imprecise_arbitrary phi =
  SL.map (function
    | psi when SL.is_pure psi -> SL.mk_and [psi; SL.emp]
  ) phi

let to_imprecise phi = match SL.as_query phi with
  | SymbolicHeap_SAT psi -> to_imprecise_sh psi
  | SymbolicHeap_ENTL (lhs, rhs) -> SL.mk_gneg (to_imprecise_sh lhs) (to_imprecise_sh rhs)
  | _ -> to_imprecise_arbitrary phi

(** Imprecise -> Precise *)

let rec to_precise_sh phi = match SL.view phi with
  | Eq _ | Distinct _ | Pure _ -> SL.mk_star [phi; SL.tt]
  | Exists (xs, psi) -> SL.mk_exists xs (to_precise_sh psi)
  | _ ->
    SL.map_view
      (fun psi -> match psi with
        | And psis -> SL.mk_star @@ BatList.remove_if SL.is_emp psis
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
  | Star psis -> List.for_all SL.is_atom psis
  | And psis ->
    let _, spatial = List.partition SL.is_pure psis in
    begin match spatial with
    | [] -> true
    | xs -> List.for_all SL.is_symbolic_heap xs
    end

  | _ -> is_spatial_part phi

let as_imprecise_sh phi =
  assert (is_imprecise_sh phi);
  let atoms = SL.select_subformulae SL.is_atom phi in
  let es = SL.bound_vars phi in
  let spatial, pure = List.partition SL.is_spatial_atom atoms in
  es, spatial, pure

let as_imprecise_query phi = match SL.view phi with
  | _ when is_imprecise_sh phi -> SL.SymbolicHeap_SAT phi
  | GuardedNeg (lhs, rhs) when List.for_all is_imprecise_sh [lhs; rhs] ->
    SL.SymbolicHeap_ENTL (lhs, rhs)
  | _ -> SL.Arbitrary phi

let is_existential_sh phi = match SL.view phi with
  | Exists (xs, psi) -> is_imprecise_sh psi
  | _ -> is_imprecise_sh phi

(** TODO: existential symbolic heaps *)
let to_precise phi = match SL.view phi with
  | _ when is_existential_sh phi ->
    Logger.debug "Processing as a symbolic heap\n";
    to_precise_sh phi
  | GuardedNeg (lhs, rhs) when is_existential_sh lhs && is_existential_sh rhs ->
    Logger.debug "Processing as entailment of symbolic heaps\n";
    SL.mk_gneg (to_precise_sh lhs) (to_precise_sh rhs)
  | _ ->
    Logger.debug "Processing as arbitrary formula\n";
    to_precise_arbitrary phi
