(* Implementation of search of small models of inductive defintions.
 *
 * The abstract is based on the *base abstraction* as defined in: <TODO>.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open InductivePredicate

module Logger = Logger.MakeWithDir
  (struct let dirname = "small-models" let name = "small-models" let level = 1 end)

module Abstraction = struct

  type t = SL_graph.t list

  let mk g = [g]

  let join a1 a2 = a1 @ a2

  let join_list xs = List.fold_left join xs []

  let equal as1 as2 =
    let equal_one a1 a2 =
      let alloc1 = SL_graph.get_vertices a1 in
      let alloc2 = SL_graph.get_vertices a2 in
      List.equal SL.Term.equal alloc1 alloc2
    in
    List.equal equal_one as1 as2

end

(** Graph based *)

let remove_pred phi =
  SL.map_view (function
    | Predicate _ -> SL.emp
  ) phi
  (*
  | Star (psis) when List.for_all SL.is_atom psis -> List.split SL.is_predicate
  | _ -> raise @@ SolverUtils.UnsupportedFragment "Complex inductive predicate"
  *)

let unfold_one pred case =
  SL.map_view (function
    | Predicate (name, xs, _) when name = pred.name ->
      InductivePredicate.instantiate pred xs
  ) case

let initial_states predicate =
  predicate.base_cases
  |> List.map SL_graph.compute
  |> List.map Abstraction.mk
  |> List.concat

let compute_edge (parent, child) data =
  child.inductive_cases
  |> List.map (unfold_one parent)
  |> List.map remove_pred
  |> List.map SL_graph.compute
  |> List.map Abstraction.mk
  |> List.concat

(** Dependency graph *)
module G = struct
  module Self = Graph.Persistent.Digraph.ConcreteBidirectional(InductivePredicate)
  include Self

  module Dot = Graph.Graphviz.Dot
    (struct
      include Self
      let graph_attributes _ = []
      let default_vertex_attributes v = []
      let vertex_name id = id.name
      let vertex_attributes v = []
      let get_subgraph _ = None
      let edge_attributes e = []
      let default_edge_attributes _ = []
    end)

  let compute () =
    SID.fold_on_user_defined (fun pred acc ->
      let g = add_vertex acc pred in
      let children = SID.dependencies pred in
      Format.printf "%s : %d\n" pred.name (List.length children);
      List.fold_left (fun g child ->
        Format.printf "%s -> %s\n" pred.name child.name;
        add_edge g pred child
      ) g children
    ) empty

  let output filename g =
    let channel = open_out filename in
    Dot.output_graph channel g;
    close_out channel

end

module Fixpoint = Graph.Fixpoint.Make(G)
  (struct
    type g = G.t
    type vertex = G.V.t
    type edge = G.E.t

    type data = Abstraction.t
    let direction = Graph.Fixpoint.Forward
    let equal = Abstraction.equal
    let join = Abstraction.join
    let analyze = compute_edge
  end)


let compute_fixpoint dependency_graph =
  Fixpoint.analyze initial_states dependency_graph

let dump_small_models g get_models =
  G.iter_vertex (fun v ->
    let models = get_models v in
    List.iteri (fun i g -> Logger.dump SL_graph.G.output_file (Format.asprintf "%s%d.dot" v.name i) g) models
  ) g

[@@@ ocaml.warning "-5"]

let compute () =
  let g = G.compute () in
  Logger.dump G.output "predicate_graph.dot" g;
  dump_small_models g (compute_fixpoint g)
