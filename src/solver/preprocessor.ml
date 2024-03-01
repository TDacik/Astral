(* Top-level preprocessor.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL

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

let broom_preprocessing phi =
  (* TODO: Remove dependency on Options *)
  if Options_base.broom_preprocessing ()
  then
    let sl_graph = SL_graph.compute phi in
    let phi = BroomPreprocessing.apply sl_graph phi in
    Debug.formula ~suffix:"0-broom_preprocessing" phi;
    phi
  else phi

let first_phase context =
  let phi, vars = Context.get_input context in

  let phi = broom_preprocessing phi in

  let phi = SSL.normalise phi in
  Debug.formula ~suffix:"1-normalisation" phi;

  let phi = rewrite_semantics phi in
  Debug.formula ~suffix:"2-semantics_rewriting" phi;

  Context.set_preprocessed context phi vars

(** ==== 2nd phase ==== *)

let remove_useless_vars phi vars =
  (* TODO: Remove dependency on Options *)
  if SSL.is_positive phi || Options_base.ignore_unused_vars () then
    let phi_vars = SSL.get_vars phi in
    let vars = List.filter (fun v -> List.mem v phi_vars) vars in
    if List.mem Variable.nil phi_vars then Variable.nil :: vars
    else vars
  else vars

let antiprenexing phi =
 (* TODO: Remove dependency on Options *)
  if Options_base.antiprenexing () then
    let phi = Antiprenexing.apply phi in
    let phi = Simplifier.simplify phi in
    Debug.formula phi ~suffix:"5-antiprenexing";
    phi
  else phi

let second_phase_aux aggresive context =
  let phi, vars = Context.get_input context in

  let phi = PurePreprocessing.apply phi in
  Debug.formula ~suffix:"3-pure_folding" phi;

  let phi = QuantifierElimination.apply context.sl_graph phi in
  Debug.formula phi ~suffix:"4-quantifier_elimination";

  let phi = Simplifier.simplify phi in
  Debug.formula phi ~suffix:"5-simplification";

  let phi = antiprenexing phi in
  Debug.formula phi ~suffix:"6-antiprenexing";

  let phi, vars =
    if aggresive then
      let phi,vars = AggresiveSimplifier.simplify context.sl_graph phi in
      Debug.formula phi ~suffix:"7-aggresive-simp";
      phi, vars
    else phi, vars
  in

  let vars = remove_useless_vars phi vars in
  Context.set_preprocessed context phi vars

let second_phase context = match Options_base.preprocessing () with
  | `None -> context
  | `Default -> second_phase_aux false context
  | `Aggresive -> second_phase_aux true context


