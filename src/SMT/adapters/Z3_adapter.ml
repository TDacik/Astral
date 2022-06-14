(* Z3 adapter for Astral
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Adapter_sig

type t = Z3.Expr.expr

let context = ref (Z3.mk_context [])

(** Translation of SMT expressions *)
let rec translate = function
  | SMT.Variable (x, _) -> failwith ""

  | SMT.True -> Z3.Boolean.mk_true !context
  | SMT.False -> Z3.Boolean.mk_false !context
  | SMT.And es -> Z3.Boolean.mk_and !context (List.map translate es)
  | SMT.Or es -> Z3.Boolean.mk_or !context (List.map translate es)
  | SMT.Not e -> Z3.Boolean.mk_not !context (translate e)
  | SMT.Implies (e1, e2) -> Z3.Boolean.mk_implies !context (translate e1) (translate e2)

  | SMT.Union sets -> Z3.Set.mk_union !context (List.map translate sets)
  | SMT.Inter sets-> Z3.Set.mk_intersection !context (List.map translate sets)


let solve (phi : SMT.term) =
  let solver = Z3.Solver.mk_simple_solver ctx in
  match Solver.check solver [translate phi] with
  | Solver.SATISFIABLE -> SMT.Sat
  | Solver.UNSATISFIABLE -> SMT.Unsat
  | Solver.UNKNOWN -> SMT.Unknown
