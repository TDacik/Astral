open SMT
open Translation_context
open ReachabilityEncoding

open Translation_sig

module Make(Locations : LOCATIONS) = struct

  let reach_back ctx dom y x =
    let cond = Boolean.mk_and [Set.mk_mem x dom; Boolean.mk_distinct x y] in
    let next = mk_succ ctx x in
    let prev_next = Array.mk_select ctx.heap_prev next in
    Boolean.mk_implies cond (Boolean.mk_eq x prev_next)

  let semantics_empty ctx dom fp x y f l bound =
    Boolean.mk_and [
      Boolean.mk_eq x l;
      Boolean.mk_eq y f;
      Set.mk_eq_empty dom
    ]

  let semantics_non_empty ctx dom fp x y f l bound =
    let reach_next = reach ctx x y bound in
    let reach_prev = Locations.mk_forall ctx.locs_sort (reach_back ctx dom y) in
    let domain_def = Boolean.mk_eq dom fp in
    let first = Boolean.mk_eq (Array.mk_select ctx.heap_prev x) f in
    let last = Boolean.mk_eq (Array.mk_select ctx.heap y) l in
    let first_fp = Boolean.mk_not (Set.mk_mem f dom) in
    let last_fp = Boolean.mk_not (Set.mk_mem l dom) in
    Boolean.mk_and [
      Boolean.mk_distinct x l;
      Boolean.mk_distinct y f;
      reach_next;
      reach_prev;
      domain_def;
      first; first_fp;
      last; last_fp;
    ]

  let semantics ctx dom fp x y f l bound =
    Boolean.mk_or [
      semantics_empty ctx dom fp x y f l bound;
      semantics_non_empty ctx dom fp x y f l bound
    ]

  (** The footprint of doubly-linked list is the domain of the path from x to l. *)
  let axioms ctx fp x y f l bound =
    let pi = Set.mk_fresh_var "pi" ctx.fp_sort in
    Boolean.mk_or [
      Boolean.mk_and [
        Boolean.mk_eq x l;
        Boolean.mk_eq y f;
        Set.mk_eq_empty fp;
      ];
      Boolean.mk_and [
        Boolean.mk_or [Boolean.mk_distinct x l; Boolean.mk_distinct y f];
        path ctx pi x y bound;
        Set.mk_eq fp (Set.mk_add pi y);
      ]
    ]

end
