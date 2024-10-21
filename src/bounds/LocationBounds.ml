(* Computation of location bounds.
 *
 * TODO: - tighter bounds for formulae without predicates?
 *       - tighter chunk size
 *       - atomic formulae
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL

(** Representation of single bound entry *)

module SortBound = struct

  module Self = struct

    type t = {
      allocated : Int.t;
      total : Int.t;
    }

    let show bound = Format.asprintf "(allocated: %d, total: %d)" bound.allocated bound.total

  end

  include Self
  include Datatype.Printable(Self)

  let init allocated total = {allocated = allocated; total = total}

  let zero = init 0 0

  let n x = init x x

  let plus b1 b2 = init (b1.allocated + b2.allocated) (b1.total + b2.total)

end

open SortBound

(** Representation of location bounds *)

include Sort.MonoMap(SortBound)

let allocated sort self = (find sort self).allocated

let total sort self = (find sort self).total

(** Computation of location bounds *)

(** Get all unique variables of a sort *)
let sort_vars sort g phi =
  SSL.get_vars_sort sort phi
  |> BatList.unique ~eq:(SL_graph.must_eq g)

let chunk_size x =
  let sort = SSL.Variable.get_sort x in
  if Sort.is_loc sort then 2.0
  else if Sort.is_dls sort then 1.5
  else if Sort.is_nls sort then 2.0
  else assert false

let chunk_size_small x = chunk_size x -. 1.0 (* Minus anonymous location *)

let var_alloc_size chunk_size_fn g phi x =
  let sort = SSL.Variable.get_sort x in
  if SL_graph.must_pointer_any g x then 1.0
  else chunk_size_fn x

let compute_allocated chunk_size_fn sort g phi =
  sort_vars sort g phi
  |> List.map (var_alloc_size chunk_size_fn g phi)
  |> BatList.fsum
  |> Float.floor
  |> Float.to_int

let compute_total chunk_size_fn sort g phi =
  let vars = sort_vars sort g phi in
  compute_allocated chunk_size_fn sort g phi

let compute_positive sort g phi =
  if Sort.is_nil sort then SortBound.init 0 1
  else
  let chunk_size, bonus = match SSL.classify_fragment phi with
    | Atomic | SymbolicHeap_SAT -> chunk_size_small, 0
    | SymbolicHeap_ENTL -> chunk_size_small, 1
    | Positive | Arbitrary -> chunk_size, 0
  in
  let allocated = compute_allocated chunk_size sort g phi in
  let total = compute_total chunk_size sort g phi in
  init (allocated + bonus) (total + bonus)

let rec garbage_chunk_bound = function
  | And (psi1, psi2) | GuardedNeg (psi1, psi2) | Or (psi1, psi2) ->
    max (garbage_chunk_bound psi1) (garbage_chunk_bound psi2)
  | Not psi -> garbage_chunk_bound psi
  | Star psis -> BatList.sum @@ List.map garbage_chunk_bound psis
  | Septraction (_, psi2) -> garbage_chunk_bound psi2
  | Exists (_, psi) -> garbage_chunk_bound psi
  | Emp | PointsTo _ | LS _ | DLS _ | NLS _ -> 1
  | Eq _ | Distinct _ | Pure _ -> 1 (* TODO: can be improved if not under negation? *)
  | other -> failwith @@ SSL.show other

let add_chunk_size bounds phi =
  let ls_bound =
    try find Sort.loc_ls bounds
    with Not_found -> SortBound.zero (* Or use other sort? *)
  in
  let garbage_bound = garbage_chunk_bound phi in
  let new_bound = SortBound.plus ls_bound (SortBound.n garbage_bound) in
  add Sort.loc_ls new_bound bounds

let compute phi g =
  let sorts = Sort.loc_nil :: SSL.get_loc_sorts phi in
  let positive_bounds =
    List.fold_left
      (fun acc sort ->
        let bound = compute_positive sort g phi in
        add sort bound acc
      ) empty sorts
  in
  match SSL.classify_fragment phi with
    | Arbitrary -> add_chunk_size positive_bounds phi
    | _ -> positive_bounds
