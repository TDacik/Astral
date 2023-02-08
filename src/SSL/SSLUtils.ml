(* Visualisation of formula AST
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL

module G = struct

  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
  (struct
    include String
    let hash = Hashtbl.hash
  end)
  (struct
    type t = [`Left | `Right]
    let compare = Stdlib.compare
    let default = `Left
  end)

  include G
  include Graph.Oper.P(G)

end

module AST = struct

  type t = G.t * G.V.t

  let show_seq = function
    | [x] -> SSL.show x
    | xs -> "<" ^ (List.map SSL.show xs |> String.concat ", ") ^ ">"

  let is_syntactic_sugar phi = is_true phi || is_false phi || is_emp phi

  let node_name phi psi =
    let label =
      (* Syntactic sugar *)
      if is_true psi then "true"
      else if is_false psi then "false"
      else if is_emp psi then "emp"
      else match psi with
      | Var _ | Pure _ -> SSL.show psi
      | And (f1, f2) -> "∧"
      | Or (f1, f2) -> "∨"
      | Not f -> "¬"
      | GuardedNeg (f1, f2) -> "∧¬"
      | Star (f1, f2) -> "★"
      | Septraction (f1, f2) -> "--(★)"
      | LS (v1, v2) -> Format.asprintf "ls(%a, %a)" SSL.pp v1 SSL.pp v2
      | DLS (v1, v2, v3, v4) -> Format.asprintf "dls(%a, %a, %a, %a)"
                                  SSL.pp v1 SSL.pp v2 SSL.pp v3 SSL.pp v4
      | PointsTo (v1, vs) -> Format.asprintf "%a ↦ %s" SSL.pp v1 (show_seq vs)

      | Eq [x; y] -> Format.asprintf "%a = %a" SSL.pp x SSL.pp y
      | Eq xs -> Format.asprintf "equals(%s)" (String.concat ", " @@ List.map SSL.show xs)

      | Distinct [x; y] -> Format.asprintf "%a ≠ %a" SSL.pp x SSL.pp y
      | Distinct xs ->
          Format.asprintf "distinct(%s)" (String.concat ", " @@ List.map SSL.show xs)

      | Exists (xs, _) -> Format.asprintf "∃ %s" (String.concat ", " @@ List.map SSL.show xs)
      | Forall (xs, _) -> Format.asprintf "∀ %s" (String.concat ", " @@ List.map SSL.show xs)

    in
    Format.asprintf "\"%d: %s\"" (SSL.subformula_id phi psi) label

  let make_leaf v = (G.add_vertex G.empty v, v)

  let make_unary_node (tree, root) v =
    let tree = G.add_vertex tree v in
    let tree = G.add_edge_e tree (v, `Left, root) in
    (tree, v)

  let make_binary_node (lhs, lhs_root) (rhs, rhs_root) v =
    let tree = G.union lhs rhs in
    let tree = G.add_vertex tree v in
    let tree = G.add_edge_e tree (v, `Left, lhs_root) in
    let tree = G.add_edge_e tree (v, `Right, rhs_root) in
    (tree, v)

  let rec create phi psi =
    if is_syntactic_sugar psi then
      make_leaf (node_name phi psi)
    else match SSL.node_type psi with
    | Var _ -> make_leaf @@ SSL.show psi
    | Operator _ ->
        make_leaf (node_name phi psi)
    | Connective [psi'] | Quantifier (_, psi') ->
        let tree' = create phi psi' in
        make_unary_node tree' (node_name phi psi)
    | Connective [psi1; psi2] ->
        let lhs = create phi psi1 in
        let rhs = create phi psi2 in
        make_binary_node lhs rhs (node_name phi psi)

end

let id = ref (-1)

module Dot = Graph.Graphviz.Dot
  (struct
    include G

    let show_label = function `Left -> "L" | `Right -> "R"

    let graph_attributes _ = [`Rankdir `TopToBottom]
    let default_vertex_attributes _ = []
    let vertex_name v = v
    let vertex_attributes _ = []
    let get_subgraph _ = None
    let edge_attributes e =
      let label = show_label @@ E.label e in
      [`Dir `None; `Label label]
    let default_edge_attributes _ = []
end)

let out_ast path phi =
  (*let phi = RichSyntax.sugarize phi in*)
  let g = fst (AST.create phi phi) in
  let channel = open_out path in
  Dot.output_graph channel g;
  close_out channel
