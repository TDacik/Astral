(* Top-level preprocessor.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SL
open Context

module Logger = Debug.QueryDir(struct
  let dirname = "preprocessor"
  let name = "preprocesssor"
  let level = 1
end)

let counter = ref 0

type pass = (Context.t -> Context.t) * string

let apply ctx ((fn, name) : pass) =
  let ctx' = fn ctx in
  Profiler.add @@ "- " ^ name;
  counter := !counter + 1;
  let name = Format.asprintf "%d-%s" !counter name in
  Logger.sl_formula name ctx'.phi;
  ctx'

let apply_list = List.fold_left apply

(** Remove unused definitions (sorts, defs, predicates). Variables are handled separately
    because of additions constraints of strong-separation logic. *)
let remove_unused_elements ?(with_vars=false) ctx =
  let is_used_var v = BatList.mem_cmp SL.Variable.compare v (SL.get_vars ctx.phi) in
  let is_used_sort s = BatList.mem_cmp Sort.compare s (SL.get_all_sorts ctx.phi) in
  let is_used_def d = BatList.mem_cmp MemoryModel.StructDef.compare d (SLID.get_structs ctx.phi) in
  let is_used_pred p =
    SL.exists (fun phi -> match SL.view phi with
      | Predicate (name, _, _) -> String.equal name p
      | _ -> false
    ) ctx.phi
  in

  let filter_heap_sort heap_sort sorts =
    HeapSort.to_list heap_sort
    |> List.filter (fun (dom, _) -> BatList.mem_cmp Sort.compare dom sorts)
    |> HeapSort.of_list
  in

  let vars = List.filter is_used_var ctx.vars in
  let sorts = List.filter is_used_sort ctx.sorts in
  let defs = List.filter is_used_def ctx.defs in
  let heap_sort = filter_heap_sort ctx.heap_sort sorts in
  let inductive_preds = List.filter is_used_pred ctx.inductive_preds in

  {ctx with sorts; heap_sort; defs; inductive_preds}


(** ==== 1st phase ==== *)

(** It is crucial that this pass is run in the first phase because if affects fragment
    classification which is needed to compute bounds. *)
let rewrite_semantics ctx =
  if not @@ Config.ImprecisePureAtoms.get () then ctx
  else
    let phi = PreciseToImprecise.to_precise ctx.phi in
    {ctx with phi = phi}

let first_phase context =
  counter := 0;

  BaseLogic.use_simplification false;

  apply_list context [
    NegationNormalisation.apply_ctx, "normalisation";
    rewrite_semantics, "semantics_rewriting";
    Inlining.inline_ctx, "inlining";
  ]

(** ==== 2nd phase ==== *)

let remove_useless_vars phi vars =
  if SL.is_positive phi then
    let phi_vars = SL.free_vars phi in
    let vars = List.filter (fun v -> List.mem v phi_vars) vars in
    if List.mem Variable.nil phi_vars then Variable.nil :: vars
    else vars
  else vars

let second_phase_aux context =
  let vars = remove_useless_vars context.phi context.vars in
  let ctx' = Context.set_preprocessed context context.phi vars in

  apply_list ctx' [
    Simplifier.simplify_ctx, "simplification";
    (*AggresiveSimplifier.apply_ctx, "simplification 2";*)
    QuantifierElimination.apply_ctx, "quantifier_elim";
    (*Simplifier.normalise_heap_terms, "simplification2";*)
    EntailmentSimplifier.apply_ctx, "entailment_simpl";
    GlobalSID.formula_preprocessing_ctx, "builtins";
  ]
  (*in
  let sl_graph = SL_graph.compute ctx2.phi in
  let bounds = LocationBounds.compute ctx2.phi ctx2.raw_input.heap_sort sl_graph in
  let ctx2 = {ctx2 with location_bounds = bounds} in (* TODO: take min? *)*)

let second_phase context =
  if Config.Preprocessing.get ()
  then second_phase_aux context
  else context

(** ==== 3rd phase ==== *)

let default_bound_map phi =
  let module BoundMap = SL.MonoMap(SL.Term.MonoList) in
  let dangling = SLID.may_dangling_terms phi in
  Logger.debug "Globally syntactically dangling terms: %a\n" SL.Term.pp_list dangling;
  let predicates = SL.select_subformulae SL.is_predicate phi in
  BoundMap.of_list @@ List.map (fun p -> (p, dangling)) predicates

let third_phase ?bound_map ctx =
  let bound_map = Option.value bound_map ~default:(default_bound_map ctx.phi)in
  let res = remove_unused_elements @@ apply_list ctx [
    UnfoldIDs.apply_ctx ~bound_map, "pred_unfolding";
    QuantifierElimination.apply_ctx, "q_elim_2";
  ]
  in
  if not @@ SL.is_quantifier_free res.phi
  then {res with quantifiers = Some "yes"}
  else if Option.is_some res.quantifiers
  then {res with quantifiers = Some "eliminated"}
  else res
