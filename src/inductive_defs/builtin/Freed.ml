(* Freed as a built-in predicate.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module Self =  struct

  let name = "freed"
  let signature = [Sort.loc_nil]

  let default_instantiation = []
  let instantiate _ [x] = Result.ok (SL.mk_predicate "freed" [x])

  let must_allocated [x] = [x]

  let unique_footprint = true

  let struct_defs = []
  let heap_sort = HeapSort.empty

  let term_bound _ _ _ = Float.one

  let is_present phi =
    SL.exists (fun psi -> match SL.view psi with
      | Predicate (name, _, _) when String.equal name "freed" -> true
      | _ -> false
    ) phi

  let additional_bound phi =
    if is_present phi then
      LocationBounds0.init_sort Sort.loc_nil 0 1 (* Location representing freed memory *)
    else LocationBounds0.empty

  let sl_graph _ = SL_graph.empty (* TODO *)
  let rules _ = []

  let count_freed phi =
    let is_freed phi = match SL.view phi with Predicate ("freed", _, _) -> true | _ -> false in
    SL.select_subformulae is_freed phi
    |> List.length

  let global_preprocessing phi = match SL.as_query phi with
   | SymbolicHeap_ENTL (lhs, rhs) ->
     let nb_freed_lhs = count_freed lhs in
     let nb_freed_rhs = count_freed rhs in
     if Int.equal nb_freed_lhs nb_freed_rhs then phi
     else lhs
   | _ -> phi

  module Bound = struct
    type t = unit
    let show () = ""
    let compute _ _ _ _ = ()
  end

  module Translation (E : Translation_sig.ENCODING) = struct

    open E

    let translate (ctx : E.Context.t) ([xt], []) domain [x] () =
      let open SMT in
      let freed = Locations.mk_var ctx.locs "freed" in
      let fields =
        MemoryModel.StructDef.get_fields @@ HeapSort.find_target (SL.Term.get_sort xt) ctx.heap_sort
      in
      let field_semantics =
        List.map (fun f -> SMT.mk_eq [HeapEncoding.mk_succ ctx.heap f x; freed]) fields
      in
      let semantics = Boolean.mk_and @@ Sets.mk_eq_singleton domain x :: field_semantics in
      let axioms =
        List.map (fun v -> SMT.mk_distinct [SMT.of_var v; freed]) ctx.smt_vars
        |> SMT.mk_and
      in
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
