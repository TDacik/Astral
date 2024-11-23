(* Encoding of locations using bitvectors.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT
open Location_sig

open SortBound

module Self = struct

  let name = "bitvectors"

  type internal = {
    nb_locs : int;
    bv_width : int;
  }

  type t = internal Location_sig.locs

  let show self =
    Format.asprintf "#locs: %d, width: %d" self.internal.nb_locs self.internal.bv_width

  (** {2 Initialization} *)

  let compute_width bounds = LocationBounds.sum bounds

  let init_constants bounds width =
    List.init (width + 1) (fun i -> Bitvector.mk_const_of_int i width)

  let init_encoding bounds width =
    let null :: constants = init_constants bounds width in
    let set_sort = SMT.Sets.mk_sort @@ Bitvector.mk_sort width in
    fst @@ LocationBounds.fold (fun sort bound (res, remaining) ->
      if Sort.is_nil sort then (res, remaining)
      else
        let consts, rest = BatList.takedrop bound.total remaining in
        let encoded_sort = SMT.Sets.mk_var (Sort.name sort) set_sort in
        Sort.Map.add sort (encoded_sort, null :: consts) res, rest
    ) bounds (Sort.Map.empty, constants)

  let init heap_sort bounds =
    let width = compute_width bounds in
    let sort = Bitvector.mk_sort width in
    let internal = {
        nb_locs = LocationBounds.sum bounds;
        bv_width = width;
      }
    in
    let null = Bitvector.mk_zero width in
    let constants = init_constants bounds width in
    let encoding = init_encoding bounds width in
    LocationUtils.init internal bounds heap_sort sort null constants encoding

  (** === Axioms === *)

  let heap_axioms self heap =
    let max_bv = Bitvector.mk_const_of_int (self.internal.nb_locs - 1) self.internal.bv_width in
    BatList.range 0 `To (self.internal.nb_locs - 1)
    |> List.map (fun i -> Bitvector.mk_const_of_int i self.internal.bv_width)
    |> List.map (fun bv -> Array.mk_select heap bv)
    |> List.map (fun term -> Bitvector.mk_lesser_eq term max_bv)
    |> Boolean.mk_and

  (** === Lemmas === *)

  let lemmas _ = Boolean.tt

end

include LocationBuilder.Make(Self)
