(* Graphical output of formulae in .dot format.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open MemoryModel

open BaseLogic_terms

module U = UnicodeSymbols

module Application = BaseLogic_application

(** Representation of a formula node. *)
module Vertex = struct

  type t = Application.t Option.t * Int.t Option.t * String.t [@@deriving compare, equal]


  let hash = Hashtbl.hash

  let show (_, _, name) = name

  let show_full (_, tag, name) = match tag with
    | None -> name
    | Some i -> Format.asprintf "\"%d: %s\"" i name

  let app (app, _, _) = app

end


(** Integer as edge labels should ensure that children are sorted as intended. *)
module Edge = struct
  include Int
  let default = 0
end

module G = struct
  module Self = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge)
  include Self
  include Graph.Oper.P(Self)
end

let edge_label e = match Vertex.app @@ G.E.src e, G.E.label e with
  | Some IfThenElse, 0 -> "cond"
  | Some IfThenElse, 1 -> "then"
  | Some IfThenElse, 2 -> "else"
  | _, n -> string_of_int n

module DotConfig = struct
  include G
  let graph_attributes _ = [`Rankdir `TopToBottom]
  let default_vertex_attributes _ = []
  let vertex_name v = Vertex.show_full v
  let vertex_attributes v = [`Label (Vertex.show v)]
  let get_subgraph _ = None

  let default_edge_attributes _ = []
  let edge_attributes e = [
      `Arrowhead `None;
      `Label (edge_label e);
    ]
end

module Dot = Graph.Graphviz.Dot(DotConfig)

type ast = G.t

(** Pretty application *)

type action =
  | Stop of string (* Show string and do not continue with sub-trees *)
  | Continue of string (* Show string and continue with sub-trees *)
  | ContinueWith of string * t list (* Continue with modified sub-trees *)

let rec fold_heap_term field ?(n=1) = function
  | Application (HeapTerm field', [x]) when Field.equal field field' ->
    fold_heap_term field ~n:(n+1) x
  | x ->
    let folded = Format.asprintf "%s^%d" (Field.show field) n in
    if is_var x then Stop (Format.asprintf "%s[%s]" folded (show x))
    else ContinueWith (folded, [x])

let rec fold_select arr ?(n=1) = function
  | Application (Select, [arr'; x]) when equal arr arr' ->
    fold_select arr ~n:(n+1) x
  | x ->
    let n = if n = 1 then "" else "^" ^ string_of_int n in
    if is_var arr && is_var x then Stop (Format.asprintf "%s%s[%s]" (show arr) n (show x))
    else if is_var arr then ContinueWith (Format.asprintf "%s%s[.]" (show arr) n, [x])
    else if is_var x then ContinueWith (Format.asprintf "[.]%s[%s]" n (show x), [arr])
    else Continue (Format.asprintf "[.]%s[.]" n)

(** TODO:
    - make the construction bottom-up for even prettier printing
    - constant sets *)
let pretty_node_name = function
  (* Introduce syntax sugar *)
  | Application (HeapTerm f, [x]) -> fold_heap_term f x
  | Application (Select, [arr; index]) -> fold_select arr index

  | Application (Equal, [x; y]) when is_var x && is_var y ->
    Stop (Format.asprintf "%s %s %s" (show x) !U.eq (show y))

  | Application (Distinct, [x; y]) when is_var x && is_var y ->
    Stop (Format.asprintf "%s %s %s" (show x) !U.neq (show y))

  | Application (Constructor def, _)  ->
    Continue (StructDef.show_cons def)
    (*Stop (Format.asprintf "%s(%s)" (StructDef.show_cons def) (show_list xs))*)

  | Application (Predicate (name, _), xs) when List.for_all is_var xs ->
    Stop (Format.asprintf "%s(%s)" (Identifier.show name) (show_list xs))

  | Application (Not, [Application (Emp, [])]) ->
    Stop (Format.asprintf "%s emp" !U.not)

  | Application (Enum _, []) -> Stop !U.empty_set
  | Application (Equal, _) -> Continue !U.eq
  | Application (Distinct, _) -> Continue !U.neq
  | Application (And, _) -> Continue !U.and_
  | Application (Or, _) -> Continue !U.or_
  | Application (Not, _) -> Continue !U.not
  | Application (GuardedNot, _) -> Continue (!U.and_ ^ !U.not)
  | Application (PointsTo, _) -> Continue !U.maps_to
  | Application (Star, _) -> Continue !U.star
  | Application (Septraction, _) -> Continue !U.septraction

  | Binder (Exists _, xs, _) -> Continue (!U.exists ^ Variable.show_list xs)
  | Binder (Forall _, xs, _) -> Continue (!U.forall ^ Variable.show_list xs)
  | Binder (Exists2 _, xs, _) -> Continue (!U.exists ^ "2" ^ Variable.show_list xs)
  | Binder (Forall2 _, xs, _) -> Continue (!U.forall ^ "2" ^ Variable.show_list xs)

  | Application (app, _) -> Continue (Application.show app)
  | Variable _ -> assert false

let to_ast ?(dagify=false) term =
  (* Create single node *)
  let mk_node ?app kind n = if dagify then (app, None, kind) else (app, Some n, kind) in

  (* Recursively builds sub-trees *)
  let rec builder tag = function
    | Variable v ->
      let node = mk_node (Variable.show v) tag in
      let g = G.add_vertex G.empty node in
      (g, node, tag + 1)
    | node ->
      let name, sub_trees = match pretty_node_name node with
        | Stop str -> str, []
        | Continue str -> str, get_operands node
        | ContinueWith (str, sub) -> str, sub
      in
      (* Construct sub-trees *)
      let root = match node with
        | Application (app, _) -> mk_node ~app name tag
        | _ -> mk_node name tag
      in
      let g, next_tag = BatList.fold_lefti (fun (acc, tag) i x ->
        let g, root', next_tag = builder tag x in
        let acc = G.union g acc in
        (G.add_edge_e acc (root, i, root'), next_tag)
      ) (G.add_vertex G.empty root, tag + 1) sub_trees
      in
      (g, root, next_tag)
  in
  match term with
    | Application (GuardedNot, [lhs; rhs]) ->
      let root = mk_node ~app:Application.GuardedNot !U.entails 0 in
      let lhs, lhs_root, next_tag  = builder 1 lhs in
      let rhs, rhs_root, _ = builder next_tag rhs in
      let g = G.union lhs rhs in
      let g = G.add_edge_e g (root, 0, lhs_root) in
      G.add_edge_e g (root, 1, rhs_root)

    | _ -> match builder 0 term with (ast, _, _) -> ast

let output_ast path g =
  let channel = open_out path in
  Dot.output_graph channel g;
  close_out channel
