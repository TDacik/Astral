(* Encoding of list-segment predicate.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

open SSL
open SMT

open PredicateBounds.Entry

open Context_sig
open PredicateEncoding_sig

module Build (P : PREDICATE_BASE_ENCODING) = struct

  include P
  open Context

  let to_var ctx (SSL.Var (name, _)) = SMT.Variable.mk (Identifier.show name) ctx.loc_sort

  let translate ctx predicate domain =
    let x, y = match predicate with LS (x, y) -> (to_var ctx x, to_var ctx y) in
    let bounds = Bounds.predicate_bound ctx.bounds predicate in
    let heap = HeapEncoding.next ctx.heap in

    (P.semantics ctx [x; y] [heap] domain bounds,
     Boolean.mk_true (),
     P.footprints ctx [x; y] [heap] domain bounds)

end


module Default (Context : CONTEXT) = struct

  open ReachabilityEncoding.Make(Context)
  open Context

  include Build(struct

    module Context = Context

    let name = "default"

    let semantics ctx [x; y] [heap] domain (LS_bound bound) =
      let reach = reach heap x y bound in
      let path = Set.mk_eq domain (path ctx heap x y bound) in
      let types = Locations.mk_set_of_type ctx.locs domain Sort.loc_ls in
      Boolean.mk_and [reach; path; types]

    let footprints ctx [x; y] [heap] domain (LS_bound bound) = [path ctx heap x y bound]

  end)

end

  (*
  (* == SH-encoding == *)

  module SymbolicHeaps = Build(struct

    let semantics ctx [x; y] [heap] domain bounds =
      let cond1 = Boolean.mk_eq x y in
      let cond2 = Boolean.mk_and [
        Boolean.mk_distinct x y;
        Boolean.mk_eq (mk_next ctx x) y;
      ]
      in
      let case1 = Boolean.mk_and [cond1; Set.mk_eq_empty fp] in
      let case2 = Boolean.mk_and [cond2; Set.mk_eq_singleton fp x ] in
      Boolean.mk_or [case1; case2]

    let axioms _ _ _ = Boolean.mk_true ()

    let footprints _ fp _ = [fp]

  end)
  *)
