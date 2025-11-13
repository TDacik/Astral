(** Preprocessing *)

open InductiveDefinition

(** Create Logger module *)
let make_logger pred =
  let name = name pred in
  let module Logger =
    Debug.QueryDir(struct
      let dirname = "preds/" ^ name
      let name = name
      let level = 2
    end)
  in
  (module Logger : Debug_sig.EXTENDED_LOGGER)


let preprocess_cases fn pred = InductiveDefinition.map_cases fn pred

let rewrite_semantics phi =
  if Config.ImprecisePureAtoms.get ()
  then PreciseToImprecise.to_precise phi
  else phi

let rec repeat_until_fixpoint ~eq f x =
  let x' = f x in
  if eq x x' then x
  else repeat_until_fixpoint ~eq f x'

let normalise (pred : t) =
  let module Logger = (val make_logger pred : Debug_sig.EXTENDED_LOGGER) in
  (* Before checking, we need to eliminate quantifiers *)
  if Inlining.can_be_inlined pred.name then
    let _ = Logger.debug "Removing predicate\n" in
    None
  else
    let _ = Logger.debug "Keeping predicate\n" in
   let _ = Logger.inductive_predicate pred in

  let pred = preprocess_cases rewrite_semantics pred in
  let _ = Logger.inductive_predicate ~name:(pred.name ^ "_3-semantics-rewrite") pred in

  (*let pred = refresh pred in
  Logger.dump pred "_2-refresh";
*)

  Some pred

let preprocess (pred : t) =
  let module Logger = (val make_logger pred : Debug_sig.EXTENDED_LOGGER) in

  let qelim case = QuantifierElimination.apply (SL_graph.compute case) case in
  let pred = preprocess_cases qelim pred in
  Logger.inductive_predicate ~name:(pred.name ^ "_4-quntifier-elim") pred;

  let pred = InductiveDefinition.map Simplifier.simplify pred in
  Logger.inductive_predicate ~name:(pred.name ^ "_5_simplifier") pred;

  (* Needs to be last as it introduces disjunctive rules *)
  let pred = RuleAntiunification.apply pred in
  Logger.inductive_predicate ~name:(pred.name ^ "_6_generalisation") pred;

  let forbidden_vars = GlobalSID.existentials pred in
  let pred = InductiveDefinition.map (repeat_until_fixpoint ~eq:SL.equal @@ IntroduceIfThenElse.apply ~forbidden_vars) pred in
  Logger.inductive_predicate ~name:(pred.name ^ "_7_ite_intro") pred;
  Some pred
