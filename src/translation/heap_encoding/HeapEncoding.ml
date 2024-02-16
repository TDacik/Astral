(* Representation of heap encoding.
 *
 * TODO: Translation of strong-separation semantics axioms should only use field arrays
 *       that are relevant.
 *
 * TODO: Heap axioms for heaps introduced by septractions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SMT

open Location_sig

module Make (Locations : LOCATIONS) = struct

  let name = "arrays"

  module Locations = Locations

  type t = {
    locs : Locations.t;
    next : SMT.Term.t;
    prev : SMT.Term.t;
    top : SMT.Term.t;
  }

  let next self = self.next
  let prev self = self.prev
  let top self = self.top

  let mk ?(suffix="") phi locs =
    let loc_sort = Locations.get_sort locs in
    let heap_sort = SMT.Array.mk_sort loc_sort loc_sort in
    {
      locs = locs;
      next = SMT.Array.mk_var ("next" ^ suffix) heap_sort;
      prev = SMT.Array.mk_var ("prev" ^ suffix) heap_sort;
      top = SMT.Array.mk_var ("top" ^ suffix) heap_sort;
    }

  let elems self = [self.next; self.prev; self.top]

  (** Auxiliary functions *)

  let mk_succ selector x = SMT.Array.mk_select selector x

  let mk_next self x = mk_succ self.next x

  let mk_prev self x = mk_succ self.prev x

  let mk_top self x = mk_succ self.top x


  let mk_image_selector self domain selector =
    let locs = Locations.get_constants self.locs in
    let fp_sort = Locations.set_sort self.locs in
    let terms =
      List.map
        (fun loc ->
          Boolean.mk_ite
            (Set.mk_mem loc domain)
            (Set.mk_singleton @@ mk_succ selector loc)
            (Set.mk_empty fp_sort)
        ) locs
    in
    Set.mk_union terms fp_sort

  let mk_image self domain =
    let images = List.map (mk_image_selector self domain) (elems self) in
    Set.mk_union images (Locations.set_sort self.locs)

  let mk_locs self domain =
    let fp_sort = Locations.set_sort self.locs in
    Set.mk_union [mk_image self domain; domain] fp_sort

  let mk_eq_on_domain_selector locs domain lhs rhs =
    Locations.mk_forall locs
      (fun loc ->
        let in_domain = Set.mk_mem loc domain in
        let lhs_select = Array.mk_select lhs loc in
        let rhs_select = Array.mk_select rhs loc in
        let select_eq = Boolean.mk_eq lhs_select rhs_select in
        Boolean.mk_implies in_domain select_eq
      )

  let mk_eq_on_domain lhs rhs domain =
    List.map2 (mk_eq_on_domain_selector lhs.locs domain) (elems lhs) (elems rhs)
    |> Boolean.mk_and

  let mk_strongly_disjoint heap vars domains =
    let fp_sort = Locations.set_sort heap.locs in
    (* Locations that are common for at least two heaps. *)
    let common_locs =
      List.map (mk_locs heap) domains
      |> List_utils.diagonal_product
      |> List.map (fun (locs1, locs2) -> Set.mk_inter [locs1; locs2] fp_sort)
      |> (fun xs -> Set.mk_union xs fp_sort)
    in
    let stack_image = Set.mk_enumeration fp_sort vars in
    Set.mk_subset common_locs stack_image

  (** Inverse translation *)

  (* TODO: refactor. *)

  let inverse_aux_ls self model heap src next =
    StackHeapModel.Heap.add_ls
      heap
      ~src
      ~next:(Locations.inverse_translate self.locs model next)

  let inverse_aux_dls self model heap src next prev =
    StackHeapModel.Heap.add_dls
      heap
      ~src
      ~next:(Locations.inverse_translate self.locs model next)
      ~prev:(Locations.inverse_translate self.locs model prev)

  let inverse_aux_nls self model heap src next top =
    StackHeapModel.Heap.add_nls
      heap
      ~src
      ~next:(Locations.inverse_translate self.locs model next)
      ~top:(Locations.inverse_translate self.locs model top)

  let inverse_translate self model domain =
    SMT.Model.eval model domain
    |> SMT.Set.get_elems
    |> List.fold_left
        (fun heap loc ->
          let next = SMT.Model.eval model (mk_next self loc) in
          let loc' = Locations.inverse_translate self.locs model loc in
          let sort = StackHeapModel.Location.get_sort loc' in
          if Sort.is_ls sort then inverse_aux_ls self model heap loc' next
          else if Sort.is_dls sort then
            let prev = SMT.Model.eval model (mk_prev self loc) in
            inverse_aux_dls self model heap loc' next prev
          else if Sort.is_nls sort then
            let top = SMT.Model.eval model (mk_top self loc) in
            inverse_aux_nls self model heap loc' next top
          else Utils.internal_error "Model allocates nil"
        ) StackHeapModel.Heap.empty

  (** === Axioms === *)

  (* Axioms enforced by location encoding *)
  let heap_axioms self =
    [self.next; self.prev; self.top]
    |> List.map (Locations.heap_axioms self.locs)
    |> Boolean.mk_and

  (* Axioms enforced by heap encoding, currently none *)
  let axioms self = heap_axioms self

end
