(* Model validation for symbolic heaps.
 *
 * TODO: dls & nls
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SSL
open Context

module SH = StackHeapModel
open SH

module Print = Printer.Make(struct let name = "Model checker" end)

(** Compute footprints according the unique footprint property *)
let rec compute_footprint sh phi = match phi with
  | Eq _ | Distinct _ -> Footprint.empty
  | PointsTo (Var x, _) -> Footprint.singleton (Stack.find x sh.stack)
  | LS (Var x, Var y) ->
    let src = Stack.find x sh.stack in
    let dst = Stack.find y sh.stack in
    if Location.equal src dst then Footprint.empty
    else Footprint.of_list (SH.get_path sh ~src ~dst)
  | Star psis ->
    let fps = List.map (compute_footprint sh) psis in
    Footprint.of_list (List.concat @@ List.map Footprint.elements fps)

let rec check sh phi =
  let stack = sh.stack in
  let heap = sh.heap in
  match phi with
  | Eq xs ->
    List.map (fun (Var x) -> Stack.find x stack) xs
    |> List_utils.all_equal Location.equal

  | Distinct xs ->
    List.map (fun (Var x) -> Stack.find x stack) xs
    |> List_utils.all_distinct Location.equal

  | PointsTo (Var x, Node.LS_t n) ->
      let expected_n = Stack.find n stack in
      let n = Heap.find_field Next (Stack.find x stack) heap in
      Location.equal n expected_n

  | PointsTo (Var x, Node.DLS_t (n, p)) ->
      let expected_n = Stack.find n stack in
      let expected_p = Stack.find p stack in
      let n = Heap.find_field Field.Next (Stack.find x stack) heap in
      let p = Heap.find_field Field.Prev (Stack.find x stack) heap in
      Location.equal n expected_n && Location.equal p expected_p

  | PointsTo (Var x, Node.NLS_t (t, n)) ->
      let expected_n = Stack.find n stack in
      let expected_t = Stack.find t stack in
      let n = Heap.find_field Field.Next (Stack.find x stack) heap in
      let t = Heap.find_field Field.Top (Stack.find x stack) heap in
      Location.equal n expected_n && Location.equal t expected_t

  | LS (Var x, Var y) ->
    let src = Stack.find x stack in
    let dst = Stack.find y stack in
    SH.has_path sh ~src ~dst

  | Star psis -> List.for_all (check sh) psis

let check_star sh psis =
  let fps = List.map (compute_footprint sh) psis in
  let fp = List.fold_left Footprint.union Footprint.empty fps in
  let domain = SH.domain sh in
  let disjoint = (*TODO: ListUtils.are_disjoint fps*) true in
  let semantics = List.for_all (check sh) psis in
  semantics && disjoint && Footprint.equal fp domain

let check_symbolic_heap_entailment sh lhs rhs =
  let sat_lhs = check_star sh lhs in
  let sat_rhs = check_star sh rhs in
  sat_lhs && not @@ sat_rhs


(** ====  Top-level function ==== **)

let check sh phi = match SSL.as_query phi with
  | SymbolicHeap_SAT psis -> check_star sh psis
  | SymbolicHeap_ENTL (lhs, rhs) -> check_symbolic_heap_entailment sh lhs rhs
  | _ -> failwith "Model checking is available only for symbolic heaps"

let verify_model context =
  let sh = Option.get context.model in
  if check sh context.phi
  then Printf.printf "[Model checker] Model verified\n"
  else let _ = Printf.printf "[Model checker] Incorrect model\n" in exit 1
