(* Representation of heap encoding.
 *
 * TODO: Translation of strong-separation semantics axioms should only use field arrays
 *       that are relevant.
 *
 * TODO: Heap axioms for heaps introduced by septractions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SMT
open MemoryModel
open Location_sig

module SH = StackHeapModel

module Logger = Logger.Make(struct let name = "Heap encoding" let level = 2 end)

module Make (Locations : LOCATIONS) = struct

  let name = "arrays"

  module Locations = Locations

  type t = {
    heap_sort : HeapSort.t;

    locs : Locations.t;             (* Underlying location encoding. *)
    field_map : SMT.t Field.Map.t; (* Mapping of field to their array encoding. *)
  }

  let show self =
    Format.asprintf "  Heap sort: %s\n  %s\n"
      (HeapSort.show self.heap_sort)
      (Field.Map.show_custom Field.show_with_sort SMT.show_with_sort self.field_map)

  let mk_field suffix field locs =
    let name = Format.asprintf "%s%s" (Field.show field) suffix in
    let sort = Field.get_sort field in
    let heap_sort = SMT.Array.mk_sort locs.sort locs.sort in
    SMT.Array.mk_var name heap_sort

  (** Construct the encoding of a heap.

      For each field, we create an array with sort Loc -> Loc, i.e., original sorts of fields
      are ignored. *)
  let mk ?(suffix="") phi heap_sort locs =
    let fields = HeapSort.get_fields heap_sort in
    let field_map = List.fold_left (fun acc f ->
      let arr = mk_field suffix f locs in
      Field.Map.add f arr acc
    ) Field.Map.empty fields
    in
    {heap_sort; locs; field_map}

  (** Encoding *)

  let field_encoding self field =
    try Field.Map.find field self.field_map
    with Not_found -> failwith @@ Field.show_with_sort field

  let get_fields self : Field.t list = Field.Map.keys self.field_map

  let get_arrays self : SMT.t list = Field.Map.values self.field_map

  (** Auxiliary functions *)

  let mk_succ self field x =
    let arr = field_encoding self field in
    SMT.Array.mk_select arr x

  let mk_image_selector self domain selector =
    let fp_sort = Locations.set_sort self.locs in
    let terms =
      List.map
        (fun loc ->
          Boolean.mk_ite
            (Sets.mk_mem loc domain)
            (Sets.mk_singleton @@ mk_succ self selector loc)
            (Sets.mk_empty fp_sort)
        ) self.locs.constants
    in
    Sets.mk_union fp_sort terms

  let mk_image self domain =
    let images = List.map (mk_image_selector self domain) (get_fields self) in
    Sets.mk_union (Locations.set_sort self.locs) images

  let mk_locs self domain =
    let fp_sort = Locations.set_sort self.locs in
    Sets.mk_union fp_sort [mk_image self domain; domain]

  let mk_eq_on_domain_selector locs domain lhs rhs =
    Locations.mk_forall locs
      (fun loc ->
        let in_domain = Sets.mk_mem loc domain in
        let lhs_select = Array.mk_select lhs loc in
        let rhs_select = Array.mk_select rhs loc in
        let select_eq = Boolean.mk_eq [lhs_select; rhs_select] in
        Boolean.mk_implies in_domain select_eq
      )

  let mk_eq_on_domain lhs rhs domain =
    let lhs_arr = List.map (field_encoding lhs) @@ get_fields lhs in
    let rhs_arr = List.map (field_encoding rhs) @@ get_fields rhs in
    List.map2 (mk_eq_on_domain_selector lhs.locs domain) lhs_arr rhs_arr
    |> Boolean.mk_and

  let mk_strongly_disjoint heap vars domains =
    let fp_sort = Locations.set_sort heap.locs in
    (* Locations that are common for at least two heaps. *)
    let common_locs =
      List.map (mk_locs heap) domains
      |> List_utils.diagonal_product
      |> List.map (fun (locs1, locs2) -> Sets.mk_inter fp_sort [locs1; locs2])
      |> Sets.mk_union fp_sort
    in
    let stack_image = Sets.mk_constant fp_sort vars in
    Sets.mk_subset common_locs stack_image

  (** Inverse translation *)

  (** TODO: move part of this to LocationBuilder *)
  let translate_value loc sort heap fields self model =
    let locs = List.map (fun field ->
        let sort = Field.get_sort field in
        Locations.inverse_translate self.locs model
        (SMT.Model.eval model (mk_succ self field loc))
      ) fields
    in
    let def = HeapSort.find_target sort self.heap_sort in
    StackHeapModel.Value.mk_struct def locs

  let inverse_translate_loc self model (heap : StackHeapModel.Heap.t) (loc_term, loc) =
    Logger.debug "Translating heap for '%s'\n" (SH.Location.show_with_sort loc);
    let source_sort = SH.Location.get_sort loc in
    let target_sort = HeapSort.find_target source_sort self.heap_sort in

    Logger.debug "  Source sort %s\n" (Sort.show source_sort);
    Logger.debug "  Target sort %s\n" (StructDef.show target_sort);
    let fields = StructDef.get_fields target_sort in
    Logger.debug "  Relevant fields are: %s\n"
      (String.concat ", " @@ List.map Field.show_with_sort fields);

    let target = translate_value loc_term source_sort heap fields self model in
    StackHeapModel.Heap.add loc target heap

  let inverse_translate self model domain =
    List.fold_left (inverse_translate_loc self model) SH.Heap.empty domain

  (** === Axioms === *)

  (* Axioms enforced by location encoding *)
  let heap_axioms self =
    get_arrays self
    |> List.map (Locations.heap_axioms self.locs)
    |> Boolean.mk_and

  (* Axioms enforced by heap encoding, currently none *)
  let axioms self = heap_axioms self

end
