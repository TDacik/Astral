(* Model validation for symbolic heaps.
 *
 * Currently, only formulae with the unique fooptrint property are supported.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SL
open MemoryModel

module SH = StackHeapModel
open SH

module Print = Logger.Make(struct let name = "Model checker" let level = 2 end)

type error =
  | Unsupported of string     (* Formula is in unsupported fragment *)
  | Failure of string *string (* Internal failure: exception, backtrace *)

(** Compute footprints according the unique footprint property *)
let rec compute_footprint sh phi = match SL.view phi with
  | Emp | Eq _ | Distinct _ -> Footprint.empty
  | PointsTo (x, _, _) -> Footprint.singleton (Stack.eval sh.stack x)
  | Predicate (id, xs, defs) -> SID.compute_footprints id (xs, defs) sh
  | Star psis ->
    let fps = List.map (compute_footprint sh) psis in
    Footprint.of_list (List.concat @@ List.map Footprint.elements fps)

(** Check whether the formula holds somewhere inside the model. *)
let rec check sh phi = match SL.view phi with
  | Emp -> true
  | Eq xs ->
    List.map (Stack.eval sh.stack) xs
    |> List_utils.all_equal Location.equal

  | Distinct xs ->
    List.map (Stack.eval sh.stack) xs
    |> List_utils.all_distinct Location.equal

  | PointsTo (x, s, ys) ->
    let sx = SH.eval sh x in
    let sys = List.map (SH.eval sh) ys in
    let fields = StructDef.get_fields s in
    Heap.mem sx sh.heap
    && BatList.for_all2 (fun field y -> Location.equal y @@ Heap.find_field field sx sh.heap)
         fields sys

  | Predicate (id, xs, defs) -> SID.model_check id (xs, defs) sh

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
  if not @@ SLID.has_unique_footprint phi then
    Result.error (Unsupported "Model checker expects formula with unique footprint")
  else try begin match SL.as_query phi with
    | SymbolicHeap_SAT psis -> Result.ok @@ check_star sh psis
    | SymbolicHeap_ENTL (lhs, rhs) -> Result.ok @@ check_symbolic_heap_entailment sh lhs rhs
    | _ -> Result.error @@ (Unsupported "Model checker expects symbolic heap")
  end
  (* We want to continue even when model checking fails *)
  with e ->
    let msg = Printexc.to_string e in
    let backtrace = Printexc.get_backtrace () in
    Result.error @@ Failure (msg, backtrace)
