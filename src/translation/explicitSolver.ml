(* Incremental instantiation of quantifiers
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT

open Quantifiers.Quantifier

let expand context phi q =
  let cons, acc, x, range = match q with
    | Forall (x, range) -> Boolean.mk_and, Boolean.mk_true (), x, range
    | Exists (x, range) -> Boolean.mk_or, Boolean.mk_false (), x, range
  in
  List.fold_left
    (fun acc v ->
      let instance = SMT.substitute phi x v in
      cons [acc; instance]
    ) acc range

(** Solve by replacing all quantifiers with boolean connectives *)
let solve context phi qs =
  let phi = List.fold_left (fun acc q -> expand context acc q) phi qs in
  let solve_fn = match Options.backend () with
    | "cvc5" -> Cvc5_adapter.solve
    | "z3" -> Z3_adapter.solve
  in
  match solve_fn phi with
  | SMT.SMT_Sat ->
      let model = None (*Solver.get_model solver*) in
      (model, true)
  | _ -> (None, false)