(* Visualisation of formula AST
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open RichSyntax

module G = struct

  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
  (struct
    include String
    let hash = Hashtbl.hash
  end)
  (struct
    type t = [`Left | `Right | `None]
    let compare = Stdlib.compare
    let default = `Left
  end)

  include G
  include Graph.Oper.P(G)

end

module AST = struct

  type t = G.t * G.V.t

  let show_seq = function
    | [x] -> show x
    | xs -> "<" ^ (List.map show xs |> String.concat ", ") ^ ">"

  let node_name top phi psi =
    let label = match RichSyntax.of_ssl psi with
      | True -> "true"
      | False -> "false"
      | Emp -> "emp"
      | Implies (_, _) -> "→"
      | Iff (_, _) -> "↔"
      | Var _ | Pure _ -> SSL.show psi
      | And (f1, f2) -> "∧"
      | Or (f1, f2) -> "∨"
      | Not f -> "¬"
      | GuardedNeg (f1, f2) when top -> "⊨"
      | GuardedNeg (f1, f2) -> "∧¬"
      | Star _ -> "★"
      | Septraction (f1, f2) -> "--(★)"
      | LS (v1, v2) -> Format.asprintf "ls(%a, %a)" pp v1 pp v2
      | DLS (v1, v2, v3, v4) -> Format.asprintf "dls(%a, %a, %a, %a)"
                                  pp v1 pp v2 pp v3 pp v4
      | PointsTo (v1, vs) -> Format.asprintf "%a ↦ %s" pp v1 (show_seq vs)

      | Eq [x; y] -> Format.asprintf "%a = %a" pp x pp y
      | Eq xs -> Format.asprintf "equals(%s)" (String.concat ", " @@ List.map show xs)

      | Distinct [x; y] -> Format.asprintf "%a ≠ %a" pp x pp y
      | Distinct xs ->
          Format.asprintf "distinct(%s)" (String.concat ", " @@ List.map show xs)

      | Exists (xs, _) -> Format.asprintf "∃ %s" (String.concat ", " @@ List.map show xs)
      | Forall (xs, _) -> Format.asprintf "∀ %s" (String.concat ", " @@ List.map show xs)

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

  let mk_variadic_node trees v =
    let tree0 = G.add_vertex G.empty v in
    let tree =
      List.fold_left
        (fun acc (tree, root) ->
          let g = G.union tree acc in
          G.add_edge_e g (v, `None, root)
        ) tree0 trees
    in
    (tree, v)

  let rec create ?(top=false) phi psi =
    if SSL.is_true psi then make_leaf "true"
    else if SSL.is_false psi then make_leaf "false"
    else if SSL.is_emp psi then make_leaf "emp"
    else if SSL.is_implies psi then
      let lhs, rhs = SSL.get_implies_operands psi in
        let lhs = create phi lhs in
        let rhs = create phi rhs in
        make_binary_node lhs rhs (node_name top phi psi)
    else if SSL.is_iff psi then
      let lhs, rhs = SSL.get_iff_operands psi in
        let lhs = create phi lhs in
        let rhs = create phi rhs in
        make_binary_node lhs rhs (node_name top phi psi)
    else match SSL.node_type psi with
    | Var _ -> make_leaf @@ SSL.show psi
    | Operator _ ->
        make_leaf (node_name top phi psi)
    | Connective [psi'] | Quantifier (_, psi') ->
        let tree' = create phi psi' in
        make_unary_node tree' (node_name top phi psi)
    | Connective [psi1; psi2] ->
        let lhs = create phi psi1 in
        let rhs = create phi psi2 in
        make_binary_node lhs rhs (node_name top phi psi)
    | Connective psis ->
        let trees = List.map (create phi) psis in
        mk_variadic_node trees (node_name top phi psi)

end

module Dot = Graph.Graphviz.Dot
  (struct
    include G

    let show_label = function `Left -> "L" | `Right -> "R" | `None -> ""

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
  let g = fst (AST.create ~top:true phi phi) in
  let channel = open_out path in
  Dot.output_graph channel g;
  close_out channel
