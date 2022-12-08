(* Computation of model bounds:
 *
 *  1. location bound     ... minimal cardinality of location sort
 *  2. list-length bounds ... interval <min, max> for a length of each ls predicate
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open Translation_context

module Printer = Printer.Make (struct let name = "Bounds" end)

let partition_equality g x y =
  if SL_graph.must_eq g x y then true
  else SSL.Variable.equal x y

let rec struct_stack_bound phi = match phi with
  | Eq _ | Neq _ | PointsTo _ | LS _ -> Variable.Set.of_list (get_vars phi)
  | Not psi -> struct_stack_bound psi
  | And (psi1, psi2) -> Variable.Set.inter  (struct_stack_bound psi1) (struct_stack_bound psi2)
  | GuardedNeg (psi1, _) -> struct_stack_bound psi1
  | Or (psi1, psi2) -> Variable.Set.union (struct_stack_bound psi1) (struct_stack_bound psi2)
  | Star (psi1, psi2) -> Variable.Set.union (struct_stack_bound psi1) (struct_stack_bound psi2)
  (*TODO: *)
  | Septraction (psi1, psi2) -> Variable.Set.union (struct_stack_bound psi1) (struct_stack_bound psi2)

(** Number of variables modulo must-equivalence *)
let stack_bound g phi vars =
  let alloc_vars = BatList.remove vars Variable.nil in
  let partition = BatList.unique ~eq:(partition_equality g) alloc_vars in
  let try1 = List.length partition in
  (0, try1)

let rec location_bound_atomic = function
  | And (psi1, psi2) | GuardedNeg (psi1, psi2) | Or (psi1, psi2) ->
    max (location_bound_atomic psi1) (location_bound_atomic psi2)
  | Not psi -> location_bound_atomic psi
  | Star (psi1, psi2) -> (location_bound_atomic psi1) + (location_bound_atomic psi2)
  (*TODO: check *)
  | Septraction (psi1, psi2) -> location_bound_atomic psi2
  | PointsTo _ -> 1

  (* TODO: problem with emp *)
  | Eq _ | Neq _ | Var _ ->  1

let location_bound phi g stack_bound = match SSL.classify_fragment phi with
  | SymbolicHeap_SAT -> stack_bound
  | SymbolicHeap_ENTL -> stack_bound + 1
  | Atomic -> location_bound_atomic phi
  | Positive ->
      let max = 2 * stack_bound in
      let n = SL_graph.nb_must_pointers g in
      if not @@ List.mem Variable.nil (SSL.get_vars phi)
      then max - n
      else max - n (* - 1 nil cannot have a successor *)
  (* TODO : tighter bounds for negative formulas *)
  | Arbitrary ->
      2 * stack_bound + chunk_size phi

(* Given length abstraction, location bound and two variables x and y, compute bound on
   length of list between x y *)
let rec local_bound context x y =
  if not @@ Options.list_bounds () then (0, context.location_bound)
  else
  let g = context.sl_graph in
  let _, stack_bound = stack_bound g context.phi context.vars in
  let min, max =
    if SL_graph.must_eq g x y then (0, 0)
    else if SL_graph.must_pointer g x y then (0, 1)
    else if context.polarity then
      let try1 = context.location_bound - SL_graph.nb_allocated g + 1 in
      (0, min try1 stack_bound)
    else
      let min_path, try1 = SL_graph.must_path g x y context.location_bound in
      let try2 = context.location_bound - SL_graph.nb_must_forks g in
      let try3 = try
        let ptrs, lists = SL_graph.predict_footprint g x y in
        let context' = {context with polarity = true} in
        List.length ptrs
        + (BatList.sum @@ List.map (fun phi -> match phi with LS (Var x, Var y) ->
           snd @@ local_bound context' x y) lists)
      with _ -> try2
      in
      (min_path, (min (min try1 (min try2 try3)) stack_bound))
  in
  Printer.debug "Length bound of ls(%s, %s): [%d, %d]\n"
    (Variable.show x)
    (Variable.show y)
    min max;
  (min,max)
