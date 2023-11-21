(* Encoding of doubly-linked lists.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

open SSL
open SMT

open PredicateBounds.Entry

open Context_sig
open PredicateEncoding_sig

module Build (P : PREDICATE_BASE_ENCODING) = struct

  include P
  open P.Context

  let to_var ctx (SSL.Var (name, _)) = SMT.Variable.mk name ctx.loc_sort

  let boundaries ctx = function
    | NLS (x, y, z) -> (to_var ctx x, to_var ctx y, to_var ctx z)

  let translate ctx predicate domain =
    let x, y, z = boundaries ctx predicate in
    let bounds = Bounds.predicate_bound ctx.bounds predicate in
    let next = HeapEncoding.next ctx.heap in
    let top = HeapEncoding.top ctx.heap in

    (P.semantics ctx [x; y; z] [next; top] domain bounds,
     Boolean.mk_true (),
     P.footprints ctx [x; y; z] [next; top] domain bounds)

end
module Default (Context : CONTEXT) = struct

  open ReachabilityEncoding.Make(Context)
  open Context

  include Build(struct

    module Context = Context

    let name = "default"

    let semantics ctx [x; y; z] [next; top] domain (NLS_bound (top_bound, nested_bounds)) =
      let top_reach = reach top x y top_bound in
      let top_path = path ctx top x y top_bound in
      let domain_def = Set.mk_eq domain (nested_path ctx top next x y z top_bound nested_bounds) in
      let top_loc_bound = Bounds.sort_total ctx.bounds Sort.loc_nls in
      let loc_sort = Locations.get_sort ctx.locs in
      let types = Locations.mk_set_of_type ctx.locs top_path Sort.loc_nls in

      let loc = Variable.mk_fresh "loc" loc_sort in
      let precond =
        Boolean.mk_and [
          Locations.mk_of_type ctx.locs loc Sort.loc_nls;
          Set.mk_mem loc domain
        ]
      in
      let bounds = nested_bounds.default :: nested_bounds.concrete in
      let i_bounds = Interval.meet_list bounds in
      let i_bounds = Interval.minus i_bounds (1, 1) in
      let invariant =
        typed_reach ctx next (HeapEncoding.mk_next ctx.heap loc) z Sort.loc_ls i_bounds in
      let body = Boolean.mk_implies precond invariant in
      let invariant =
        if (2 * (snd top_bound) >= top_loc_bound)
        then SMT.Quantifier.mk_forall [loc] body
        else SMT.Quantifier.mk_forall_path top x (snd i_bounds) loc body
      in
      let loc1 = Variable.mk_fresh "loc1" loc_sort in
      let loc2 = Variable.mk_fresh "loc2" loc_sort in
      let allocated_loc1 = Set.mk_mem loc1 domain in
      let allocated_loc2 = Set.mk_mem loc2 domain in
      let down_loc1 = HeapEncoding.mk_next ctx.heap loc1 in
      let down_loc2 = HeapEncoding.mk_next ctx.heap loc2 in
      let downs_eq = SMT.mk_eq down_loc1 down_loc2 in
      let distinct = SMT.mk_distinct loc1 loc2 in
      let cond = Boolean.mk_and [allocated_loc1; allocated_loc2; distinct; downs_eq] in
      let downs_not_alloc = Boolean.mk_not @@ Set.mk_mem down_loc1 domain in
      let body = Boolean.mk_implies cond downs_not_alloc in
      let nested_disjoint =
        if (2 * (snd top_bound) >= top_loc_bound)
        then SMT.Quantifier.mk_forall [loc1; loc2] body
        else
          Quantifier.mk_forall_path_nested2 top next x
          (top_bound, nested_bounds.concrete, nested_bounds.default)
          [loc1; loc2] body
      in
      let empty = Boolean.mk_and [
        SMT.mk_eq x y;
        Set.mk_eq_empty domain
      ] in
      let non_empty = Boolean.mk_and [
        top_reach;
        domain_def;
        invariant; nested_disjoint;
        types
      ] in
      Boolean.mk_or [empty; non_empty]

    let footprints (ctx : Context.t) [x; y; z] [next; top] domain (NLS_bound (top_bound, nested_bounds)) =
      [nested_path ctx top next x y z top_bound nested_bounds]

  end)

end
