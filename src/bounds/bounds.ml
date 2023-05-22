open SSL
open Translation_context

module Printer = Printer.Make (struct let name = "Bounds" end)

let must_equality g = fun x y -> SL_graph.must_eq g x y || SSL.Variable.equal x y

let stack_bound_quantifier_free sl_graph phi vars =
  let alloc_vars = BatList.remove vars Variable.nil in
  let quotient = BatList.unique ~eq:(must_equality sl_graph) alloc_vars in
  List.length quotient

let stack_bound_quantified sl_graph phi vars =
  let alloc_vars = BatList.remove (vars @ SSL.get_vars phi) Variable.nil in
  let quotient = BatList.unique ~eq:(must_equality sl_graph) alloc_vars in
  List.length quotient

let stack_bound sl_graph phi vars =
  if SSL.is_quantifier_free phi
  then stack_bound_quantifier_free sl_graph phi vars
  else stack_bound_quantified sl_graph phi vars

(** Location bound for atomic SL from paper 'A Decision Procedure for Separation Logic in SMT' *)
let rec location_bound_atomic phi = match phi with
  | And (psi1, psi2) | GuardedNeg (psi1, psi2) | Or (psi1, psi2) ->
    max (location_bound_atomic psi1) (location_bound_atomic psi2)
  | Not psi -> location_bound_atomic psi
  | Star psis -> BatList.sum @@ List.map location_bound_atomic psis
  | Septraction (_, psi2) -> location_bound_atomic psi2
  | Emp | PointsTo _ -> 1
  (* TODO: can be improved if not under negation? *)
  | Eq _ | Distinct _ | Pure _ -> 1

(** Location bound for positive formulae *)
let location_bound_positive sl_graph phi n =
    let max = 2 * n in
    let p = SL_graph.nb_must_pointers sl_graph in
    if not @@ List.mem Variable.nil (SSL.get_vars phi)
    then max - p
    else max - p (* - 1 nil cannot have a successor *)
    (* TODO: ^^^^^^ *)

let location_bound_quantifier_free sl_graph phi vars =
  let n = stack_bound_quantifier_free sl_graph phi vars in
  match SSL.classify_fragment phi with
  | SymbolicHeap_SAT -> n
  | SymbolicHeap_ENTL -> n + 1
  | Atomic ->
    let atomic = location_bound_atomic phi in
    if SSL.is_positive phi then
      let positive = location_bound_positive sl_graph phi n in
      min atomic positive
    else atomic

  | Positive -> location_bound_positive sl_graph phi n
  | Arbitrary -> (* TODO : tighter bounds for negative formulas *)
    2 * n + chunk_size phi

let location_bound_quantified sl_graph phi vars =
  let n = stack_bound_quantified sl_graph phi vars in
  match SSL.classify_fragment phi with
    | SymbolicHeap_ENTL -> n + 1
    | _ -> failwith (SSL.show phi)

let location_bound sl_graph phi vars =
  if SSL.is_quantifier_free phi
  then location_bound_quantifier_free sl_graph phi vars
  else location_bound_quantified sl_graph phi vars

(** Predicate bounds. TODO: move to separated file *)
let list_bound context x y =
  if not @@ Options.list_bounds () then (0, context.location_bound)
  else
  let g = context.sl_graph in
  let n = stack_bound context.sl_graph context.phi context.vars in
  let min, max =
    if SL_graph.must_eq g x y then (0, 0)
    else if SL_graph.must_pointer g x y && SL_graph.must_neq g x y then (1, 1)
    else if SL_graph.must_pointer g x y then (0, 1)
    else if context.polarity then
      let try1 = (context.location_bound + 1) - SL_graph.nb_allocated g in
      (0, min try1 n)
    else
      let min_path, try1 = SL_graph.must_path g x y (context.location_bound - 1) in
      let try2 = context.location_bound - SL_graph.nb_must_forks g in
      (*let try3 = try
        let ptrs, lists = SL_graph.predict_footprint g x y in
        let context' = {context with polarity = true} in
        List.length ptrs
        + (BatList.sum @@ List.map (fun phi -> match phi with LS (Var x, Var y) ->
           snd @@ list_bound context' x y) lists)
      with _ -> try2
      in*)
      (min_path, (min (min try1 try2) n))
  in
  Printer.debug "Length bound of ls(%s, %s): [%d, %d]\n"
    (Variable.show x)
    (Variable.show y)
    min max;
  (min,max)
