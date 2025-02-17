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
    Inlining.inline_ctx, "inlining";
    rewrite_semantics, "semantics_rewriting";
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

  apply_list ctx' [
    Simplifier.simplify_ctx, "simplification";
    UnfoldIDs.apply_ctx, "predicate_unfolding";
    (*Antiprenexing.apply, "antiprenexing";*)
    IntroduceIfThenElse.apply_ctx, "ite_introduction";
    QuantifierElimination.apply_ctx, "quantifier_elimination";
    Simplifier.simplify_ctx, "simplification";
    (*fun phi -> if aggresive then AggresiveSimplifier.simplify context.sl_graph phi else phi),
      "aggresive-simp";
    *)
  ]

let second_phase context = match Options_base.preprocessing () with
  | `None -> context
  | `Default -> second_phase_aux false context
  | `Aggresive -> second_phase_aux true context
