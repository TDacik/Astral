(* Encoding of nested lists.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SMT
open Translation_context
open ReachabilityEncoding

open Translation_sig

module Make(Locations : LOCATIONS) = struct

  let invariant ctx dom x y z bound =
    (fun node ->
      let pre = Set.mk_mem node dom in
      let node_invariant = reach ctx node z bound in
      Boolean.mk_implies pre node_invariant
    )

  let rec nested_path_term ctx x y z (min, max) =
    if min > max then (all_reachable_n_term {ctx with heap = ctx.heap_prev} x z min)
    else Boolean.mk_ite
      (reach_n ctx x y min)
      (all_reachable_n_term {ctx with heap = ctx.heap_prev} x z min)
      (nested_path_term ctx x y z (min + 1, max))

  let semantics ctx dom fp x y z bound =
    Boolean.mk_or [
      Boolean.mk_and [
        Boolean.mk_eq_list [x; y; z];
        Set.mk_eq_empty dom
      ];
      Boolean.mk_and [
        reach ctx x y bound;
        Locations.mk_forall ctx.locs_sort (invariant ctx dom x y z bound);
        Set.mk_eq dom (nested_path_term ctx x y z bound)
      ]
    ]

  (** The footprint of doubly-linked list is the domain of the path from x to l. *)
  let axioms ctx fp x y z bound = Boolean.mk_true ()

end
