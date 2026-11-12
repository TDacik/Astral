(* Synthesis result.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

type t = {
  precision : float;
  recall : float;
  concrete_vars : SL.Variable.t list;
  predicates : InductiveDefinition.t list;
}

let is_precise solution = not (solution.precision <> 1.0 || solution.recall <> 1.0)
