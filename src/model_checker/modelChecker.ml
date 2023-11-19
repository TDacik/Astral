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

  | PointsTo (Var x, Struct.LS_t n) ->
      let sx = Stack.find x stack in
      let sn = Stack.find n stack in
      Heap.mem sx heap && Location.equal sn @@ Heap.find_field Next sx heap

  | PointsTo (Var x, Struct.DLS_t (n, p)) ->
      let sx = Stack.find x stack in
      let sn = Stack.find n stack in
      let sp = Stack.find p stack in
      Heap.mem sx heap
      && Location.equal sn @@ Heap.find_field Next sx heap
      && Location.equal sp @@ Heap.find_field Prev sx heap

  | PointsTo (Var x, Struct.NLS_t (t, n)) ->
      let sx = Stack.find x stack in
      let sn = Stack.find n stack in
      let st = Stack.find t stack in
      Heap.mem sx heap
      && Location.equal sn @@ Heap.find_field Next sx heap
      && Location.equal st @@ Heap.find_field Top sx heap

  | LS (Var x, Var y) ->
    let src = Stack.find x stack in
    let dst = Stack.find y stack in
    SH.has_path sh ~src ~dst

  | Star psis -> List.for_all (check sh) psis

let check_star sh psis =
  let fps = List.map (compute_footprint sh) psis in
  let fp = List.fold_left Footprint.union Footprint.empty fps in
  let domain = SH.domain sh in
  let disjoint = Footprint.disjoint_list fps in
  let semantics = List.for_all (check sh) psis in
  semantics && disjoint && Footprint.equal fp domain

let check_symbolic_heap_entailment sh lhs rhs =
  let sat_lhs = check_star sh lhs in
  let sat_rhs = check_star sh rhs in
  sat_lhs && not @@ sat_rhs


(** ====  Top-level function ==== **)

let check sh phi =
  match SSL.as_query phi with
  | SymbolicHeap_SAT psis -> check_star sh psis
  | SymbolicHeap_ENTL (lhs, rhs) -> check_symbolic_heap_entailment sh lhs rhs
  | _ -> failwith "Model checking is available only for symbolic heaps"

let verify_model context =
  let sh = Option.get context.model in
  if check sh context.phi
  then Printf.printf "[Model checker] Model verified\n"
  else let _ = Printf.printf "[Model checker] Incorrect model\n" in exit 1
