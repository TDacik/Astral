(* Top-level preprocessor.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SL

let counter = ref 0

type pass = (SL.t -> SL.t) * string

let apply phi (pass : pass) =
  let fn, name = pass in
  let phi' = fn phi in
  counter := !counter + 1;
  let suffix = Format.asprintf "%d-%s" !counter name in
  Debug.formula ~suffix phi';
  phi'

let apply_list = List.fold_left apply


(** ==== 1st phase ==== *)

(** It is crucial that this pass is run in the first phase because if affects fragment
    classification which is needed to compute bounds. *)
let rewrite_semantics phi = match Options_base.semantics () with
  | `NotSpecified -> phi
  | `Precise -> phi
  | `Imprecise ->
    let phi = PreciseToImprecise.to_precise phi in
    let _ = Debug.formula ~suffix:"1.0-to_precise" phi in
    phi

let first_phase context =
  counter := 0;
  let phi, vars = Context.get_input context in

  let phi' = apply_list phi [
    NegationNormalisation.apply, "normalisation";
    Inlining.inline, "inlining";
    rewrite_semantics, "semantics_rewriting";
  ]
  in

  SID.normalise ();

  Context.set_preprocessed context phi' vars

(** ==== 2nd phase ==== *)

let remove_useless_vars phi vars =
  (* TODO: Remove dependency on Options *)
  if SL.is_positive phi || Options_base.ignore_unused_vars () then
    let phi_vars = SL.free_vars phi in
    let vars = List.filter (fun v -> List.mem v phi_vars) vars in
    if List.mem Variable.nil phi_vars then Variable.nil :: vars
    else vars
  else vars

let antiprenexing phi =
 (* TODO: Remove dependency on Options *)
  if Options_base.antiprenexing () then
    let phi = Antiprenexing.apply phi in
    (*let phi = Simplifier.simplify phi in*)
    Debug.formula phi ~suffix:"5-antiprenexing";
    phi
  else phi

let second_phase_aux aggresive context =
 let phi, vars = Context.get_input context in

  let phi' = apply_list phi [
    UnfoldIDs.apply context.location_bounds, "predicate_unfolding";
    QuantifierElimination.apply context.sl_graph, "quantifier_elimination";
    IntroduceIfThenElse.apply, "ite_introduction";
    antiprenexing, "antiprenexing";
    Simplifier.simplify ~dont_care:[], "simplification";
    (fun phi -> if aggresive then AggresiveSimplifier.simplify context.sl_graph phi else phi),
      "aggresive-simp";
  ]
  in

  let vars = remove_useless_vars phi' vars in
  Context.set_preprocessed context phi' vars

let second_phase context = match Options_base.preprocessing () with
  | `None -> context
  | `Default -> second_phase_aux false context
  | `Aggresive -> second_phase_aux true context
