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

let sum bounds =
  values bounds
  |> BatList.map (fun b -> b.total)
  |> BatList.sum

(** {2 Computation of bounds for positive formulae *)

(** Get all unique variables of a sort *)
let sort_terms sort g phi =
  SL.get_terms_of_sort sort phi
  |> BatList.unique ~eq:(SL_graph0.must_eq g)

(** Compute contribution of a single variable *)
let term_bound heap_sort g phi x =
  let sort = SL.Term.get_sort x in
  if SL_graph0.must_pointer_any g x then 1.0
  else SID.term_bound phi heap_sort x

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
  | GuardedNeg (lhs, rhs) -> max (garbage_chunk_bound lhs) (garbage_chunk_bound rhs)
  | Not psi -> garbage_chunk_bound psi
  | Star psis -> BatList.sum @@ List.map garbage_chunk_bound psis
  | Septraction (_, psi2) -> garbage_chunk_bound psi2
  | Emp | PointsTo _ | Predicate _ -> 1
  | Exists (_, psi) -> garbage_chunk_bound psi
  | Eq _ | Distinct _ | Pure _ | True | False -> 1 (* TODO: can be improved if not under negation? *)
  | _ -> Utils.internal_error ("Garbage_bound: Unexpected SL formula: " ^ SL.show phi)

let add_chunk_size bounds phi =
  let ls_bound =
    try find Sort.loc_ls bounds
    with Not_found -> SortBound.n 1 (* Or use other sort? *)
  in
  let garbage_bound = garbage_chunk_bound phi in
  let new_bound = SortBound.plus ls_bound (SortBound.n garbage_bound) in
  add Sort.loc_ls new_bound bounds

let compute phi heap_sort g =
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
  bounds
