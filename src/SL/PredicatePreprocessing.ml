(** Preprocessing *)

open InductivePredicate

let preprocess_cases fn pred =
  InductivePredicate.map_cases fn pred

let rewrite_semantics phi = match Options_base.semantics () with
  | `NotSpecified -> phi
  | `Precise -> phi
  | `Imprecise ->
    let phi = PreciseToImprecise.to_precise phi in
    (*let _ = Debug.formula ~suffix:"1.0-to_precise" phi in
    dump (pred.name ^ "_2-to_precise") pred;*)
    phi

let preprocess (pred : t) =

  let name = (name pred) in
  let module Logger = Logger.MakeWithDir(struct
    let dirname = "preds/" ^ name
    let name = name
    let level = 2
  end)
  in

  let dump name pred =
    let psi = instantiate_formals pred in
    Logger.dump SL.dump (name ^ ".smt2") psi;
    let ast = SL.to_ast psi in
    Logger.dump SL.output_ast (name ^ ".dot") ast
  in

  dump name pred;

  let pred = preprocess_cases rewrite_semantics pred in

  (*
  let pred = preprocess_cases Antiprenexing.apply pred in
  dump (pred.name ^ "_3-antiprenexing") pred;
  *)

  let qelim case = QuantifierElimination.apply (SL_graph.compute case) case in
  let pred = preprocess_cases qelim pred in
  dump (name ^ "_4-quntifier-elim") pred;

  let pred = InductivePredicate.map IntroduceIfThenElse.apply pred in
  dump (name ^ "_5-introduce-ite") pred;

  pred
