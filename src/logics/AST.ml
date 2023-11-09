(* Graph representation of formulae ASTs.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Logic_sig

module Make (Base : AST_BASE) = struct

  module Term = Base.Term

  module G = struct

    module Self = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
      (struct
        include String
        let hash = Hashtbl.hash
      end)
      (struct
        type t = [`Left | `Right | `None]
        let compare = Stdlib.compare
        let default = `Left
      end)

    include Self
    include Graph.Oper.P(Self)

    include Graph.Graphviz.Dot
      (struct
        include Self
        let graph_attributes _ = [`Rankdir `TopToBottom]
        let default_vertex_attributes _ = []
        let vertex_name v = v
        let vertex_attributes _ = []
        let get_subgraph _ = None

        let default_edge_attributes _ = []
        let edge_attributes e = []
      end)

  end

  type t = G.t

  let rec make term index =
    let name = Format.asprintf "\"%d : %s\"" index (Base.node_name term) in
    match Base.get_operands term with
      | [] -> (name, index, G.add_vertex G.empty name)
      | xs ->
        let name, (g, index) = name, BatList.fold_left
          (fun (g, index) node ->
            let child, last_index, child_tree = make node (index + 1) in
            let g =
              G.add_edge g name child
              |> G.union child_tree
            in
            (g, last_index)
          ) (G.empty, index) xs
        in
        (name, index, g)

  let make term =
    let _, _, g = make term 1 in
    g

  let dump path g =
    let channel = open_out path in
    G.output_graph channel g;
    close_out channel

end
