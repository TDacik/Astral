(* Freed as a built-in predicate.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module Self =  struct

  let name = "freed"
  let signature = [Sort.loc_nil]

  let default_instantiation = []
  let instantiate _ [x] = Result.ok (SL.mk_predicate "freed" [x])

  let unique_footprint = false

  let struct_defs = []
  let heap_sort = HeapSort.empty

  let term_bound _ _ _ = Float.one

  let is_present phi =
    SL.exists (fun psi -> match SL.view psi with
      | Predicate (name, _, _) when String.equal name "freed" -> true
      | _ -> false
    ) phi

  let additional_bound phi =
    if is_present phi then 1 (* Location representing freed memory *)
    else 0

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
      let freed = Locations.mk_var ctx.locs "freed" in
      let field_semantics =
        HeapEncoding.get_fields ctx.heap
        |> List.map (fun f -> SMT.mk_eq [HeapEncoding.mk_succ ctx.heap f x; freed])
      in
      let semantics = Boolean.mk_and @@ Sets.mk_eq_singleton domain x :: field_semantics in
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

let mk x = SL.mk_predicate "freed" [x]

let register () = GlobalSID.register_builtin (module Self : ID_sig.BUILTIN)
