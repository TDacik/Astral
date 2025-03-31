(* Freed as a built-in predicate.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module Self =  struct

  let name = "freed"
  let arity = 1

  let default_instantiation = []
  let instantiate _ [x] = Result.ok (SL.mk_predicate "freed" [x])

  let unique_footprint = false

  let struct_defs = []
  let heap_sort = HeapSort.empty

  let term_bound _ _ _ = Float.one

  let additional_bound = 1 (* Location representing freed memory *)

  let sl_graph _ = SL_graph.empty (* TODO *)
  let rules _ = []

  module Bound = struct
    type t = unit
    let show () = ""
    let compute _ _ _ _ = ()
  end

  module Translation (E : Translation_sig.ENCODING) = struct

    open E

    let translate (ctx : E.Context.t) ([_], []) domain [x] () =
      let open SMT in
      let open MemoryModel in
      let freed = Locations.mk_var ctx.locs "freed" in
      let semantics =
        Boolean.mk_and [
          Sets.mk_eq_singleton domain x;
          SMT.mk_eq [HeapEncoding.mk_succ ctx.heap Field.next x; freed];
        ]
      in
      let var_axioms =
        List.map (fun v -> SMT.mk_distinct [SMT.of_var v; freed]) ctx.smt_vars
        |> SMT.Boolean.mk_and
      in
      let nil_axiom = SMT.mk_distinct [freed; ctx.locs.null] in
      let axioms = Boolean.mk_and [var_axioms; nil_axiom] in
      let footprints = [Sets.mk_singleton x] in
      (semantics, axioms, footprints)

  end

  let preprocess _ _ = None
  let model_check _ _ = failwith "TODO"
  let compute_footprints _ _ = failwith "TODO"

end

include Self

let mk x = SL.mk_predicate "free" [x]

let register () = SID.register (module Self : ID_sig.BUILTIN)
