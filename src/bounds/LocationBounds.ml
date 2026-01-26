(* Computation of location bounds.
 *
 * TODO: - tighter bounds for formulae without predicates?
 *       - tighter chunk size
 *       - atomic formulae
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SL
open SortBound

module Logger = Logger.Make(struct let name = "Bounds" let level = 2 end)

include LocationBounds0

let to_json bounds =
  bindings bounds
  |> List.map (fun (sort, bound) -> (Sort.show sort, `String (SortBound.show bound)))
  |> (fun xs -> `Assoc xs)

(** Representation of location bounds *)

let allocated sort self = (find sort self).allocated

let total sort self = (find sort self).total

let general_sum getter bounds =
  values bounds
  |> BatList.map getter
  |> BatList.sum

let sum = general_sum (fun x -> x.total)
let sum_of_allocated = general_sum (fun x -> x.allocated)

(** {2 Computation of bounds for positive formulae *)

(** Get all unique variables of a sort *)
let sort_terms sort g phi =
  SL.get_terms_of_sort sort phi
  |> BatList.unique ~eq:(SL_graph0.must_eq g)

(** Compute contribution of a single variable *)
let term_bound heap_sort g phi x =
  let sort = SL.Term.get_sort x in
  let res =
    if SL_graph0.must_pointer_any g x then 1.0
    else GlobalSID.term_bound phi g heap_sort x
  in
  Logger.debug "[| %s |] = %f\n" (Term.show_with_sort x) res;
  res

let compute_allocated heap_sort sort g (phi : SL.t) =
  sort_terms sort g phi
  |> List.map (term_bound heap_sort g phi)
  |> BatList.fsum
  |> Float.floor
  |> Float.to_int

let compute_total heap_sort sort g phi =
  let terms = sort_terms sort g phi in
  Logger.debug "Terms of sort %a: %a\n" Sort.pp sort SL.Term.pp_list terms;
  compute_allocated heap_sort sort g phi

let compute_positive heap_sort sort g phi =
  if Sort.is_nil sort then SortBound.init 0 1
  else
  let bonus = match SL.classify_fragment phi with
    (* Bounds are compute before unfolding *)
    | _ when SLID.has_user_defined_predicates phi -> 0
    | Atomic | SymbolicHeap_SAT -> 0
    | SymbolicHeap_ENTL -> 1
    | Positive | Arbitrary -> 0
  in
  let allocated = compute_allocated heap_sort sort g phi + bonus in
  let total = compute_total heap_sort sort g phi + bonus in
  let allocated, total = match Sort.cardinality sort with
    | None -> allocated, total
    | Some bound -> min (bound - 1) allocated, min (bound - 1) total
  in
  init allocated total

(** {2 Bound for negative formulae *)

let rec garbage_chunk_bound phi = match SL.view phi with
  | And psis | Or psis -> BatList.max @@ List.map garbage_chunk_bound psis
  | GuardedNeg (lhs, rhs) -> Stdlib.max (garbage_chunk_bound lhs) (garbage_chunk_bound rhs)
  | Not psi -> garbage_chunk_bound psi
  | Star psis -> BatList.sum @@ List.map garbage_chunk_bound psis
  | Septraction (_, psi2) -> garbage_chunk_bound psi2
  | Emp | PointsTo _ | Predicate _ -> 1
  | Exists (_, psi) -> garbage_chunk_bound psi
  | Eq _ | Distinct _ | Pure _ | True | False -> 1 (* TODO: can be improved if not under negation? *)
  | _ ->
    Exceptions.internal_error
      ~reason: "Unexpected SL formula when computing location bound"
      ~details: (SL.show phi)

let add_chunk_size bounds phi =
  let ls_bound =
    try find Sort.loc_ls bounds
    with Not_found -> SortBound.n 1 (* Or use other sort? *)
  in
  let garbage_bound = garbage_chunk_bound phi in
  let new_bound = SortBound.plus ls_bound (SortBound.n garbage_bound) in
  add Sort.loc_ls new_bound bounds

let compute_general phi heap_sort g =
  let sorts = Sort.loc_nil :: HeapSort.get_loc_sorts heap_sort in
  let positive_bounds =
    List.fold_left
      (fun acc sort ->
        let bound = compute_positive heap_sort sort g phi in
        Logger.debug "Bound for sort %a is %s\n" Sort.pp sort (SortBound.show bound);
        add sort bound acc
      ) empty sorts
  in
  let bounds =
    if SL.is_positive phi then positive_bounds
    else add_chunk_size positive_bounds phi
  in
  let pred_bonus = GlobalSID.additional_bounds phi in
  let res = LocationBounds0.plus bounds pred_bonus in
  res

let default heap_sort =
  let sorts = HeapSort.get_loc_sorts heap_sort in
  List.fold_left (fun acc sort -> add sort SortBound.zero acc) empty sorts
  |> add Sort.loc_nil (SortBound.init 0 1)

(** Compute a bound for a symbolic heap without predicate calls. *)
let compute_ptr_bound heap_sort phi =
  if Option.is_some @@ SL.pointer_size phi then
    SL.select_subformulae SL.is_pointer phi
    |> List.map SL.as_pointer
    |> List.map (fun (x, _, _) -> SL.Term.get_sort x)
    |> List.fold_left (fun acc sort ->
         plus_n sort 1 acc
      ) (default heap_sort)
    |> Option.some
  else None

let compute_sh_entl phi lhs rhs heap_sort g =
  let lhs_bound = compute_ptr_bound heap_sort lhs in
  let rhs_bound = compute_ptr_bound heap_sort rhs in
  match lhs_bound, rhs_bound with
    | Some b, _ | _, Some b -> b
    | None, None -> compute_general phi heap_sort g

let rec compute_atomic psi =
  let res = match SL.view psi with
    | False -> empty
    | Eq _ | Distinct _ | Emp -> empty
    | PointsTo (x, _, _) -> add (SL.Term.get_sort x) (SortBound.init 1 1) empty
    | Star psis -> BatList.fold_left plus empty @@ List.map compute_atomic psis
    | Or psis -> BatList.fold_left LocationBounds0.max empty @@ List.map compute_atomic psis
    | Ite (_, lhs, rhs) -> LocationBounds0.max (compute_atomic lhs) (compute_atomic rhs)
    | GuardedNeg (lhs, _) -> compute_atomic lhs
    | Exists (_, psi) -> compute_atomic psi
  in
  let poly = List.length @@ SL.Term.MonoList.unique @@ SL.get_terms_of_sort Sort.loc_nil psi in
  LocationBounds0.add Sort.loc_nil (SortBound.init 0 poly) res

let compute phi heap_sort g = match SL.classify_fragment phi with
  | SymbolicHeap_ENTL ->
    let lhs, rhs = SL.as_entailment phi in
    if SL.is_atomic lhs then compute_atomic lhs
    else compute_general phi heap_sort g
  | _ -> compute_general phi heap_sort g
