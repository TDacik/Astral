(* Predicate dependency graph.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open InductiveDefinition

module G = Graph.Persistent.Digraph.ConcreteBidirectional(InductiveDefinition)
include G
include Graph.Oper.P(G)

module PathChecker = Graph.Path.Check(G)

module Dot = Graph.Graphviz.Dot
  (struct
    include G
    let graph_attributes _ = []
    let default_vertex_attributes v = []
    let vertex_name id = Format.asprintf "\"%s\"" id.name
    let vertex_attributes v = []
    let get_subgraph _ = None
    let edge_attributes e = []
    let default_edge_attributes _ = []
  end)

let is_self_recursive g pred =
  try
    let g = mirror g in
    let path_checker = PathChecker.create g in
    let succs = G.succ g pred in
    List.exists (fun succ ->
      PathChecker.check_path path_checker succ pred
    ) succs
  with Invalid_argument _ -> false

let normalise g = g
  (* TODO: keep?
   G.fold_vertex (fun v acc ->
    if is_self_recursive g v then acc
    else G.remove_vertex acc v
  ) g g
  *)

let compute sid =
  let predicates = SID0.__get_user_defined sid in
  List.fold_left (fun acc pred ->
    let g = add_vertex acc pred in
    let children = SID0.dependencies sid pred in
    List.fold_left (fun g child ->
      add_edge g pred child
    ) g children
  ) empty predicates

let has_nontrivial_cycle g =
  let module W = struct
    include G
    include Int
    let weight _ = -1
    let add = (+)
    let zero = 0 end
  in
  let module BF = Graph.Path.BellmanFord(G)(W) in
  try
    let _ =
      transitive_reduction ~reflexive:true g
      |> BF.find_negative_cycle
    in
    true
  with Not_found -> false

(** Reachability *)

module Reachability = Graph.Fixpoint.Make(G)
  (struct
    type vertex = G.V.t
    type edge = G.E.t
    type g = G.t
    type data = bool
    let direction = Graph.Fixpoint.Forward
    let equal = (=)
    let join = (||)
    let analyze _ = (fun x -> x)
  end)

let compute_dependencies g preds =
  let preds = InductiveDefinition.MonoList.unique preds in
  let module Topological = Graph.Topological.Make(G) in
  let res = Reachability.analyze (fun p -> InductiveDefinition.MonoList.mem p preds) g in
  let preds = Topological.fold (fun v acc -> if res v then v :: acc else acc) g [] in
  let res = InductiveDefinition.MonoList.unique preds in
  res

let output filename g =
  let channel = open_out filename in
  Dot.output_graph channel g;
  close_out channel
