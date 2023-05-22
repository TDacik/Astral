(* SL graphs
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Graph

module SL_edge = struct

  type t =
    | No
    | Pointer
    | List
    | Equality
    | Disequality
    [@@deriving compare, equal]

  let default = No

  let length = function
    | Pointer -> 1
    | List -> 2
    | _ -> failwith "length undefined"

  let show = function
    | No          -> "X" (* TODO *)
    | Pointer     -> "→"
    | List        -> "⇝"
    | Equality    -> "="
    | Disequality -> "≠"

  module Self = struct
    type nonrec t = t
    let compare = compare
    let show = show
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end

open SL_edge

module G = struct

  module Self = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
    (SSL.Variable)
    (SL_edge)

  include Self
  include Graph.Oper.P(Self)

  include Path.Dijkstra
    (Self)
    (struct
      type edge = Self.E.t
      type t = Int.t
      let weight e = SL_edge.length @@ Self.E.label e
      let compare = Int.compare
      let add = (+)
      let zero = 0
    end)

  include Graph.Graphviz.Dot
    (struct
      include Self

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_name = SSL.Variable.show
      let vertex_attributes v = []

      let get_subgraph _ = None
      let edge_attributes e = [
        `Label (SL_edge.show @@ E.label e);
        `Arrowhead (match E.label e with | Pointer | List -> `Normal | _ -> `None);
        `Style (match E.label e with | Pointer | List -> `Solid | _ -> `Dashed);

      ]
      let default_edge_attributes _ = []
    end)

  let output_file g path =
    let channel = open_out path in
    output_graph channel g;
    close_out channel
end

let projection g labels =
  G.fold_edges_e
    (fun (x, label, y) g ->
      if List.mem label labels then G.add_edge_e g (x, label, y)
      else g
    ) g G.empty

let projection_eq g =
  let proj = projection g [Equality] in
  let proj_mirror = G.mirror proj in
  G.union proj proj_mirror
  |> G.transitive_closure ~reflexive:true

let projection_neq g = projection g [Disequality]
let projection_pointer g = projection g [Pointer]
let projection_ls g = projection g [List]
let spatial_projection g = projection g [Pointer; List]

let must_eq g x y =
  let g = projection_eq g in
  G.mem_edge g x y

let must_neq g x y =
  let g = projection_neq g in
  G.mem_edge g x y || G.mem_edge g y x

let must_pointer g x y =
  let g = projection_pointer g in
  G.mem_edge g x y

let must_ls g x y =
  let g = projection_ls g in
  G.mem_edge g x y

let nb_must_pointers g =
  let g = projection_pointer g in
  let vertices = G.fold_edges (fun x y acc -> x :: acc) g [] in
  List.length @@ List.sort_uniq G.V.compare vertices

let must_alloc g =
  let vs = G.fold_vertex List.cons g [] in
  List.filter (fun x -> List.exists (fun y -> must_pointer g x y || (must_ls g x y && must_neq g x y)) vs) vs

let nb_allocated g =
  let alloc = must_alloc g in
  List.length alloc

let must_path g x y max =
  let g_ptr = projection_pointer g in
  try
    let edges, weight = G.shortest_path g_ptr x y in
    let _, min =
      List.fold_left
        (fun (continue, n) e ->
          if must_neq g (G.E.src e) (G.E.dst e) && must_neq g (G.E.src e) y && continue then
            (true, n+1)
          else (false, n)
        ) (true, 0) edges
    in
    (min, weight)
  with _ -> (0, max)

(* Precondition : g is already a pointer projection *)
let nb_must_preds_v g x =
  G.fold_vertex List.cons g []
  |> List.filter (fun v -> G.mem_edge g v x)
  |> List.length
  |> (fun x -> max (x-1) 0)

let nb_must_forks g =
  let g = projection_pointer g in
  G.fold_vertex List.cons g []
  |> List.map (fun v -> nb_must_preds_v g v)
  |> BatList.sum

let predict_footprint g x y =
  let g = spatial_projection g in
  let path, _ = G.shortest_path g x y in
  List.fold_left
    (fun (ptrs, lists) (x', label, y') ->
      match label with
      | Pointer -> ((SSL.PointsTo (Var x', [Var y'])) :: ptrs, lists)
      | List -> (ptrs, (SSL.LS (Var x', Var y')) :: lists)
    ) ([], []) path

(* TODO: diagonal product? *)
let all_equal xs =
  BatList.cartesian_product xs xs
  |> List.filter_map (function (SSL.Var x, SSL.Var y) -> Some (x, Equality, y) | _ -> None)
  |> List.fold_left G.add_edge_e G.empty

let all_distinct xs =
  List_utils.diagonal_product xs
  |> List.filter_map (function (SSL.Var x, SSL.Var y) -> Some (x, Disequality, y) | _ -> None)
  |> List.fold_left G.add_edge_e G.empty

let disjoint_union graphs =
  List.fold_left
    (fun acc g ->
      let must_allocs1 = must_alloc g in
      let must_allocs2 = must_alloc acc in
      let disequalities = BatList.cartesian_product must_allocs1 must_allocs2 in
      let union = G.union acc g in
      List.fold_left
        (fun g (x, y) ->
        G.add_edge_e g (x, Disequality, y)
      ) union disequalities
    ) G.empty graphs

let rec compute phi = match phi with
  | SSL.Emp -> G.empty
  | SSL.Var _ | SSL.Pure _ -> G.empty
  | SSL.Eq xs -> all_equal xs
  | SSL.Distinct xs -> all_distinct xs
  | SSL.PointsTo (SSL.Var x, [SSL.Var y]) -> G.add_edge_e G.empty (x, Pointer, y)
  | SSL.PointsTo (SSL.Var x, _) -> G.empty (* TODO *)
  | SSL.LS (SSL.Var x, SSL.Var y) -> G.add_edge_e G.empty (x, List, y)
  | SSL.DLS _ -> G.empty (* TODO *)
  | SSL.NLS _ -> G.empty (* TODO *)
  | SSL.SkipList _ -> G.empty (* TODO *)

  | SSL.Star psis -> disjoint_union (List.map compute psis)
  | SSL.And (psi1, psi2) -> G.union (compute psi1) (compute psi2)
  | SSL.Or (psi1, psi2) -> G.intersect (compute psi1) (compute psi2)
  | SSL.GuardedNeg (psi1, psi2) -> compute psi1

  | SSL.Septraction _ -> G.empty
  | SSL.Not _ -> G.empty
  | SSL.Exists _ | SSL.Forall _ -> G.empty (* TODO *)

include G
