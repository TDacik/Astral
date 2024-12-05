(* Predicate dependency graph.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open InductivePredicate

module G = Graph.Persistent.Digraph.ConcreteBidirectional(InductivePredicate)
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

let normalise g =
  G.fold_vertex (fun v acc ->
    if is_self_recursive g v then acc
    else G.remove_vertex acc v
  ) g g

let compute () =
  SID0.fold_on_user_defined (fun pred acc ->
    let g = add_vertex acc pred in
    let children = SID0.dependencies pred in
    List.fold_left (fun g child ->
      add_edge g child pred
    ) g children
  ) empty

let output filename g =
  let channel = open_out filename in
  Dot.output_graph channel g;
  close_out channel
