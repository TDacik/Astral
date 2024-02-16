(* Encoding of locations using bitvectors.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT
open Bounds

open LocationBounds.SortBound

module Self = struct

  let name = "bitvectors"

  type sort_encoding = SMT.Term.t * SMT.Term.t list

  type t = {
    nb_locs : int;
    bv_width : int;

    loc_sort : Sort.t;                         (* SMT sort of locations. *)
    sort_encodings : sort_encoding Sort.Map.t; (* Encodings of SL sorts. *)
  }

  (** {2 Initialization} *)

  let nil_const self = Bitvector.mk_zero self.bv_width

  (** Compute the smallest necessary width of bitvectors for n locations. *)
  let compute_width bounds = Bounds.sum bounds
    (*
    let n = Bounds.sum bounds in
    let log2 = (fun x -> Float.div (Float.log x) (Float.log @@ Float.of_int 2)) in
    let res =
     log2 (Float.of_int n)
      |> Float.ceil
      |> Float.to_int
    in
    if res == 0 then 1 else res
    *)

  let sort_encodings bounds width =
    let null :: consts =
      BatList.range 0 `To width
      |> List.map (fun x -> Bitvector.mk_const x width)
    in
    fst @@ LocationBounds.fold (fun sort bound (res, free_consts) ->
      if Sort.is_nil sort then (res, free_consts)
      else
        let consts = null :: BatList.take bound.total free_consts in
        let typ = SMT.Set.mk_var (Sort.show sort) (SMT.Set.mk_sort @@ Bitvector.mk_sort width) in
        Sort.Map.add sort (typ, consts) res, BatList.drop bound.total free_consts
    ) bounds (Sort.Map.empty, consts)

  let init bounds =
    let width = compute_width bounds in
    let bv_sort = Bitvector.mk_sort width in
    {
      nb_locs = Bounds.sum bounds;
      bv_width = width;
      loc_sort = bv_sort;
      sort_encodings = sort_encodings bounds.location_bounds width;
    }

  (** {2 Location signature} *)

  let get_sort self = self.loc_sort

  let get_sort_encoding self sl_sort = fst @@ Sort.Map.find sl_sort self.sort_encodings

  let get_constants self =
    BatList.range 0 `To (self.nb_locs - 1)
    |> List.map (fun x -> SMT.Bitvector.mk_const x self.bv_width)

  let get_constants_s self sl_sort = snd @@ Sort.Map.find sl_sort self.sort_encodings

  let get_index self loc =
    let consts = get_constants self in
    Option.get @@ BatList.index_of loc consts

  let inverse_translate self model loc =
    let name = SMT.Term.show loc in
    let index = get_index self loc in
    let res = Sort.Map.fold (fun sort (sort_encoding, _) acc ->
      match acc with
      | Some x -> Some x
      | None ->
        if SMT.Model.check model (Set.mk_mem loc sort_encoding)
        then Some (StackHeapModel.Location.mk index sort)
        else None
    ) self.sort_encodings None
    in
    match res with
    | Some x -> x
    | None -> StackHeapModel.Location.mk_nil 0

  (** === Axioms === *)

  let heap_axioms self heap =
    let max_bv = Bitvector.mk_const (self.nb_locs - 1) self.bv_width in
    BatList.range 0 `To (self.nb_locs - 1)
    |> List.map (fun i -> Bitvector.mk_const i self.bv_width)
    |> List.map (fun bv -> Array.mk_select heap bv)
    |> List.map (fun term -> Bitvector.mk_lesser_eq term max_bv)
    |> Boolean.mk_and

  (** === Lemmas === *)

  let lemmas _ = Boolean.mk_true ()


  let internal_repr self =
    let show (term, consts) =
      Format.asprintf "(%s, {%s})"
        (SMT.Term.show term)
        (String.concat ", " @@ List.map SMT.Term.show consts)
    in
    Sort.Map.show show self.sort_encodings

end

include LocationBuilder.Make(Self)
