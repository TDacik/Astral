(* Computation of location and list-length bounds
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open Context

module Print = Printer.Make (struct let name = "Bounds" end)

let partition_equality g x y =
  if LengthGraph.must_eq g x y then true
  else Variable.equal x y

(* TODO: lower bound *)
let stack_bound g phi vars =
  let phi_vars = SSL.get_vars ~with_nil:false phi in
  let partition = BatList.unique ~eq:(partition_equality g) phi_vars in
  (0, List.length partition)

let location_bound phi g stack_bound = match SSL.classify_fragment phi with
  | SymbolicHeap_SAT -> stack_bound
  | SymbolicHeap_ENTL
  | Positive ->
      let max = 2 * stack_bound in
      let n = LengthGraph.nb_must_pointers g in
      if not @@ List.mem Variable.Nil (SSL.get_vars phi)
      then max - n
      else max - 1 (* nil cannot have a successor *)
  (* TODO : tighter bounds for negative formulas *)
  | Arbitrary -> 4 * stack_bound

(* Given length abstraction, location bound and two variables x and y, compute bound on
   length of list between x y *)
let local_bound context x y =
  if not @@ Options.local_bounds ()  then (0, context.bound)
  else
  let g = context.length_graph in
  let min, max =
    if LengthGraph.must_eq g x y then (0, 0)
    else if LengthGraph.must_pointer g x y then (0, 1)
    else if context.polarity then (0, context.bound - LengthGraph.nb_allocated g)
    else
      let try1 = LengthGraph.must_path g x y context.bound in
      let try2 = context.bound - LengthGraph.nb_must_forks g in
      (0, min try1 try2)
  in
  Print.debug "ls %s %s -> (%d, %d)\n" (Variable.show x) (Variable.show y) min max;
  (min,max)
