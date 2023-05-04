(* Model validation for symbolic heaps.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SSL
open Context

module SH = StackHeapModel
open SH

module Print = Printer.Make(struct let name = "Model checker" end)

(*type reason_incorrect =
  | IncorrectFootprint of SSL.t
  | StarNotDisjoint of SSL.t
  | ShapeNotSatisfied of SSL.t
*)

exception StarNotDisjoint of SSL.t
(*exception IncorrectFootprint of SSL.t
*)
(** Compute footprints according the unique footprint property *)
let rec compute_footprint sh phi = match phi with
  | Eq _ | Distinct _ -> Footprint.empty
  | PointsTo (Var x, _) -> Footprint.singleton (Stack.find x sh.stack)
  | LS (Var x, Var y) ->
    let sx = Stack.find x sh.stack in
    let sy = Stack.find y sh.stack in
    if Location.equal sx sy then Footprint.empty
    else Footprint.of_list (SH.get_path sh sx sy)
  | Star psis ->
    let fps = List.map (compute_footprint sh) psis in
    if List_utils.for_all2' Footprint.disjoint fps
    then Footprint.of_list (List.concat @@ List.map Footprint.elements fps)
    else raise (StarNotDisjoint phi)

let rec check sh phi =
  let stack = sh.stack in
  let heap = sh.heap in
  match phi with
  | Eq xs ->
    List_utils.all_equal Location.equal (List.map (fun (Var x) -> Stack.find x stack) xs)
  | Distinct xs ->
    List_utils.all_distinct Location.equal (List.map (fun (Var x) -> Stack.find x stack) xs)
  | PointsTo (Var x, ys) ->
    begin try
      let expected_sys = LocationTuple.map (fun (Var y) -> Stack.find y stack) ys in
      let sys = Heap.find (Stack.find x stack) heap in
      LocationTuple.equal sys expected_sys
    with Not_found -> false
    end
  | LS (Var x, Var y) ->
    let sx = Stack.find x stack in
    let sy = Stack.find y stack in
    SH.has_path sh sx sy
  | Star psis -> List.for_all (check sh) psis

let check_symbolic_heap sh phi =
  try
    let fp = compute_footprint sh phi in
    let domain = SH.get_domain sh in
    check sh phi && Footprint.equal fp domain
  with StarNotDisjoint _ -> false

let check_symbolic_heap_entailment sh (GuardedNeg (lhs, rhs)) =
  let sat_lhs = check_symbolic_heap sh lhs in
  let sat_rhs = check_symbolic_heap sh rhs in
  sat_lhs && not @@ sat_rhs

(** Top-level function *)
let check sh phi = match SSL.classify_fragment phi with
  | SymbolicHeap_SAT -> check_symbolic_heap sh phi
  | SymbolicHeap_ENTL -> check_symbolic_heap_entailment sh phi
  | _ -> failwith "Model checking is available only for symbolic heaps"

let verify_model context =
  if Options.verify_model () && Option.is_some context.model then
    let sh = Option.get context.model in
    if check sh context.phi
    then Printf.printf "[Model checker] Model verified\n"
    else Printf.printf "[Model checker] Incorrect model\n"; exit 1
  else ();
