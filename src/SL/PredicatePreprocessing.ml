(** Preprocessing *)

open InductivePredicate

let preprocess (self : t) =

  (*
  let name = (name self) in
  let module Logger = Logger.MakeWithDir (let dirname = name let name = name let level = 2) in
  *)

  let body = instantiate_formals self in
  Debug.inductive_pred (name self) body;

  let body = PreciseToImprecise.to_precise body in
  Debug.inductive_pred ~suffix:"2-to_precise" (name self) body;

  let body = Antiprenexing.apply body in
  Debug.inductive_pred ~suffix:"3-antiprenexing" (name self) body;

  let body = IntroduceIfThenElse.apply body in
  Debug.inductive_pred ~suffix:"4-introduce-ite" (name self) body;
  (*
  let sl_graph = SL_graph.compute body in
  let body2 = QuantifierElimination.apply sl_graph body in
  Debug.inductive_pred ~suffix:"2-quntifier-elim" (name self) body2;
  *)

  InductivePredicate.mk (name self) (header self) body
