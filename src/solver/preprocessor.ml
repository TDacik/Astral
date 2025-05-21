(* Top-level preprocessor.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SL
open Context

let counter = ref 0

type pass = (Context.t -> Context.t) * string

let apply ctx ((fn, name) : pass) =
  let ctx' = fn ctx in
  counter := !counter + 1;
  let suffix = Format.asprintf "%d-%s" !counter name in
  Debug.formula ~suffix ctx'.phi;
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
let rewrite_semantics ctx = match Options_base.semantics () with
  | `NotSpecified -> ctx
  | `Precise -> ctx
  | `Imprecise ->
    let phi = PreciseToImprecise.to_precise ctx.phi in
    let _ = Debug.formula ~suffix:"1.0-to_precise" phi in
    {ctx with phi = phi}

let first_phase context =
  counter := 0;
  SID.normalise ();

  apply_list context [
    NegationNormalisation.apply_ctx, "normalisation";
    rewrite_semantics, "semantics_rewriting";
    Inlining.inline_ctx, "inlining";
  ]

(** ==== 2nd phase ==== *)

let remove_useless_vars phi vars =
  (* TODO: Remove dependency on Options *)
  if SL.is_positive phi || Options_base.ignore_unused_vars () then
    let phi_vars = SL.free_vars phi in
    let vars = List.filter (fun v -> List.mem v phi_vars) vars in
    if List.mem Variable.nil phi_vars then Variable.nil :: vars
    else vars
  else vars

let second_phase_aux aggresive context =
  let vars = remove_useless_vars context.phi context.vars in
  let ctx' = Context.set_preprocessed context context.phi vars in

  let ctx'' = apply_list ctx' [
    Simplifier.simplify_ctx, "simplification";
    AggresiveSimplifier.apply_ctx, "simplification 2";

    (* TODO: Currently, unfolding needs to be the last preprocessing to not
             break the footprint mapping. *)
    UnfoldIDs.apply_ctx, "predicate_unfolding";
    QuantifierElimination.apply_ctx, "quantifier_eliminitation";
  ]
  in
  remove_unused_elements ctx''

let second_phase context = match Options_base.preprocessing () with
  | `None -> context
  | `Default -> second_phase_aux false context
  | `Aggresive -> second_phase_aux true context
