(* Encoding of doubly-linked lists.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

open SSL
open SMT

open SortBound
open PredicateBounds.Entry

open Context_sig
open PredicateEncoding_sig

module Build (P : PREDICATE_BASE_ENCODING) = struct

  include P
  open Context

  let to_var ctx (SSL.Var (name, _)) = SMT.Variable.mk name ctx.loc_sort

  let boundaries ctx = function
    | DLS (x, y, px, ny) -> (to_var ctx x, to_var ctx y, to_var ctx px, to_var ctx ny)

  let translate ctx predicate domain =
    let x, y, px, ny = boundaries ctx predicate in
    let bounds = Bounds.predicate_bound ctx.bounds predicate in
    let next = HeapEncoding.next ctx.heap in
    let prev = HeapEncoding.prev ctx.heap in

    (P.semantics ctx [x; y; px; ny] [next; prev] domain bounds,
     Boolean.mk_true (),
     P.footprints ctx [x; y; px; ny] [next; prev] domain bounds)

end


module Default (Context : CONTEXT) = struct

  open ReachabilityEncoding.Make(Context)
  open Context

  include Build(struct

    module Context = Context

    let name = "default"

    let domain_term ctx heap x y bounds =
      Set.mk_union [path ctx heap x y bounds; Set.mk_singleton y] ctx.fp_sort

    let semantics ctx [x; y; px; ny] [next; prev] domain bounds =
      let bounds = match bounds with DLS_bound (next, prev) -> Interval.join next prev in
      let loc_sort = Locations.get_sort ctx.locs in
      let loc = SMT.Variable.mk_fresh "l" (Locations.get_sort ctx.locs) in
      let alloc = Set.mk_mem loc domain in
      let not_y = Boolean.mk_distinct loc y in
      let precond = Boolean.mk_and [alloc; not_y] in
      let conseq =
        SMT.mk_eq loc (HeapEncoding.mk_prev ctx.heap @@ HeapEncoding.mk_next ctx.heap loc)
      in
      let inv = Boolean.mk_implies precond conseq in
      let dls_loc_bound = Bounds.sort_bound ctx.bounds Sort.loc_dls in
      let invariant =
        if (2 * (snd bounds) >= dls_loc_bound.allocated)
        then Quantifier.mk_forall [loc] inv
        else Quantifier.mk_forall_path next x (snd bounds) loc inv
      in
      let empty = Boolean.mk_and [
        Boolean.mk_eq x ny;
        Boolean.mk_eq y px;
        Set.mk_eq_empty domain
      ] in
      let non_empty = Boolean.mk_and [
        Boolean.mk_distinct x ny;
        Boolean.mk_distinct y px;
        Boolean.mk_not @@ Set.mk_mem px domain; (* Predecessor of x is not allocated *)
        Boolean.mk_not @@ Set.mk_mem ny domain; (* Successor of y is not allocated *)

        Boolean.mk_eq px (HeapEncoding.mk_prev ctx.heap x);
        Boolean.mk_eq ny (HeapEncoding.mk_next ctx.heap y);

        reach next x y bounds;
        Locations.mk_subset_of_type ctx.locs domain Sort.loc_dls;
        Set.mk_eq domain (domain_term ctx next x y bounds);
        invariant
      ] in
      Boolean.mk_or [empty; non_empty]

    let footprints ctx [x; y; px; ny] [next; prev] domain bounds =
      let bounds = match bounds with DLS_bound (next, prev) -> Interval.join next prev in
      let emp = Boolean.mk_and [Boolean.mk_eq x ny; Boolean.mk_eq y px] in
      let fp =
        Boolean.mk_ite
          emp
          (Set.mk_empty ctx.fp_sort)
          (domain_term ctx next x y bounds)
      in
      [fp]

  end)

end
