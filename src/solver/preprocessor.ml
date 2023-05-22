(* Top-level preprocessor.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL

(** If phi is positive, remove all variables that does not appear in phi *)
let normalise_vars phi vars =
  let phi = Simplifier.simplify phi in (* TODO: workaround for SL-COMP *)
  if SSL.is_positive phi || Options.ignore_unused_vars () then
    let phi_vars = SSL.get_vars phi in
    let vars = List.filter (fun v -> List.mem v phi_vars) vars in
    if List.mem Variable.nil phi_vars then Variable.nil :: vars
    else vars
  else vars

let rewrite_semantics phi = match Options.semantics () with
  | `NotSpecified -> phi
  | `Precise -> phi
  (*
    let phi = PreciseToImprecise.to_precise phi in
    let _ = Debug.formula ~suffix:"1.0-to_precise" phi in
     phi
  *)
  | `Imprecise ->
    let phi = PreciseToImprecise.to_precise phi in
    let _ = Debug.formula ~suffix:"1.0-to_precise" phi in
    phi

let normalise phi vars =
  let phi = SSL.normalise phi in
  let phi = rewrite_semantics phi in
  let vars = normalise_vars phi vars in
  (phi, vars)

let preprocess context =
  (* Original formula *)
  let phi, vars = Context.get_raw_input context in
  Debug.formula ~suffix:"1-original" phi;

  (* Broom preprocessing - this phase needs to be before folding and normalisation to do not
     break iffs introduced by Broom. *)
  let phi =
    if Options.broom_preprocessing ()
    then
      let g = SL_graph.compute phi in
      let phi = BroomPreprocessing.apply g phi in
      let _ = Debug.formula phi ~suffix:"1.1-after_broom" in
      phi
    else phi
  in

  (* Folding of pure terms *)
  let phi = PurePreprocessing.apply phi in
  Debug.formula ~suffix:"2-pure_folding" phi;

  (* Normalisation *)
  let phi, vars = normalise phi vars in
  Debug.formula ~suffix:"3-normalisation" phi;

  let g =
    if Options.compute_sl_graph ()
    then SL_graph.compute phi (*|> MustAllocations.refine_graph phi*)
    else SL_graph.empty
  in

  (* Simplification I *)
  let phi = Simplifier.simplify phi in
  Debug.formula phi ~suffix:"4-simplification1";

  (* Antiprenexing *)
  let phi = Antiprenexing.apply phi in
  Debug.formula phi ~suffix:"5-antiprenexing";

  (* Simplification II *)
  let phi = Simplifier.simplify (*@@ EqualityRewritter.apply g*) phi in
  Debug.formula phi;

  Context.set_preprocessed phi vars context, g

let preprocess context =
  if Options.preprocessing ()
  then preprocess context
  else begin
    (* Only basic preprocessing *)
    let phi, vars = Context.get_raw_input context in
    Debug.formula ~suffix:"1-original" phi;
    let phi, vars = normalise phi vars in
    Debug.formula phi;
    Context.set_preprocessed phi vars context, SL_graph.empty
  end
