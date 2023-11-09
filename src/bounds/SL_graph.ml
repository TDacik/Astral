(* SL graphs
 *
 * TODO: Sl-graph normalisation
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open SSL.Field

open Graph

module SL_edge = struct

  type t =
    | No
    | Equality
    | Disequality
    | Pointer of Field.t
    | Path of Field.t
    | Disjoint of Field.t * SSL.Variable.t
    [@@deriving compare, equal]

  let default = No

  let get_field = function
    | Pointer f | Path f -> f

  let is_pointer = function
    | Pointer _ -> true
    | _ -> false

  let is_path = function
    | Path _ -> true
    | _ -> false

  let is_disjointness = function
    | Disjoint _ -> true
    | _ -> false

  let is_spatial_any = function
    | Pointer _ | Path _ -> true
    | _ -> false

  let is_spatial field = function
    | Pointer f | Path f -> Field.equal field f
    | _ -> false

  let pointers = [Pointer Next; Pointer Prev; Pointer Top]

  let paths = [Path Next; Path Prev; Path Top]

  let all_directed = pointers @ paths

  let length = function
    | Pointer _ -> 1
    | Path _ -> 2
    | _ -> failwith "length undefined"

  let show = function
    | No            -> "X" (* TODO *)
    | Pointer Next  -> "→"
    | Pointer Prev  -> "⇢"
    | Pointer Top   -> "⇒"
    | Path selector -> "⇝" ^ Field.show selector
    | Equality      -> "="
    | Disequality   -> "≠"
    | Disjoint _    -> "*"

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
        `Arrowhead (match E.label e with | Pointer _ | Path _ -> `Normal | _ -> `None);
        `Style (match E.label e with | Pointer _ | Path _ -> `Solid | _ -> `Dashed);

      ]
      let default_edge_attributes _ = []
    end)

  let output_file g path =
    let channel = open_out path in
    output_graph channel g;
    close_out channel
end

(** Projections *)

let filter g pred =
  G.fold_edges_e (fun e g -> if pred e then G.add_edge_e g e else g) g G.empty

let projection g labels = filter g (fun (_, label, _) -> List.mem label labels)

let projection_sort sort g = filter g (fun (x, _, _) -> SSL.Variable.has_sort sort x)

let projection_eq g =
  let proj = projection g [Equality] in
  let proj_mirror = G.mirror proj in
  G.union proj proj_mirror
  |> G.transitive_closure ~reflexive:true

let projection_neq g = projection g [Disequality]
let projection_pointer g = projection g SL_edge.pointers
let spatial_projection g = projection g SL_edge.all_directed
let projection_field field g = projection g [Equality; Disequality; Pointer field; Path field]


let projection_path g = projection g SL_edge.paths
let projection_path_field g = projection g [Equality; Pointer field; Path field]


let must_disjoint_with g x field y =
  G.fold_edges_e (fun e acc -> match e with
    | (x', Disjoint (field', ell), y') when
      SSL.Variable.equal x x' && SSL.Variable.equal y y' && SSL.Field.equal field field' ->
      ell :: acc
    | _ -> acc
  ) g []

let must_disjoint g field x y ell =
  BatList.mem_cmp G.V.compare ell (must_disjoint_with g x field y)

let must_eq g x y =
  let g = projection_eq g in
  G.mem_edge g x y || SSL.Variable.equal x y

let must_neq g x y =
  let g = projection_neq g in
  G.mem_edge g x y || G.mem_edge g y x

let must_pointer g x y =
  let g = projection_pointer g in
  G.mem_edge g x y

let must_pointer g field x y =
  let g = projection g [Pointer field] in
  G.mem_edge g x y

let must_pointer_any g x =
  let g = projection_pointer g in
  try
    let out_degree = G.out_degree g x in
    out_degree > 0
  (* Vertex is not in graph *)
  with Invalid_argument _ -> false

let must_root g x =
  let g = spatial_projection g in
  try
    let out_degree = G.out_degree g x in
    out_degree > 0
  (* Vertex is not in graph *)
  with Invalid_argument _ -> false

let must_path g field x y =
  let g = projection_path g in
  G.mem_edge_e g (x, Path field, y)

let must_proper_path_any g x =
  let spatial_g = projection_path g in
  try G.fold_succ (fun v res -> res || must_neq g x v) spatial_g x false
  with Invalid_argument _ -> false

let nb_must_pointers g sort =
  let g = projection_sort sort g in
  let g = projection_pointer g in
  let vertices = G.fold_edges (fun x y acc -> x :: acc) g [] in
  List.length @@ List.sort_uniq G.V.compare vertices

let must_alloc g =
  let vs = G.fold_vertex List.cons g [] in
  List.filter (fun x -> must_pointer_any g x || must_proper_path_any g x) vs

let nb_allocated g =
  let alloc = must_alloc g in
  List.length alloc

let nb_roots g =
  G.fold_vertex List.cons g []
  |> List.filter (fun x -> must_root g x)
  |> List.length

(** TODO: SL-graph normalisation *)
let must_successor_ptr g field source =
  let g = projection g [Pointer field] in
  List.hd @@ G.succ g source

let must_successor_any g field source =
  let g = projection g [Pointer field; Path field] in
  (*let g_filtered = G.fold_edges_e (fun edge acc -> match G.E.label edge with
    | Pointer _ -> G.add_edge_e acc edge
    | Path _ -> if must_neq g (G.E.src edge) (G.E.dst edge) then G.add_edge_e acc edge else acc
  ) g G.empty in*)
  List.hd @@ G.succ g source

let must_successor_edge g field source =
  let g = projection g [Pointer field; Path field] in
  List.hd @@ G.succ_e g source


let nb_joins g field =
  let g = projection g [Pointer field] in
  G.fold_vertex List.cons g []
  |> List.map (fun v -> if G.in_degree g v > 1 then G.in_degree g v - 1 else 0)
  |> BatList.sum

(** ==== SL-graph construction ==== *)

let all_equal xs =
  List_utils.diagonal_product xs
  |> List.filter_map (function (SSL.Var x, SSL.Var y) -> Some (x, Equality, y) | _ -> None)
  |> List.fold_left G.add_edge_e G.empty

let all_distinct xs =
  List_utils.diagonal_product xs
  |> List.filter_map (function (SSL.Var x, SSL.Var y) -> Some (x, Disequality, y) | _ -> None)
  |> List.fold_left G.add_edge_e G.empty

let paths g =
  let g = filter g (fun e -> SL_edge.is_spatial_any @@ G.E.label e) in
  try G.fold_edges_e List.cons g []
  with Invalid_argument _ -> []

let unpack = function (x, Pointer f, y) | (x, Path f, y) -> (x, f, y)

let update g1 g2 =
  let g = G.union g1 g2 in
  let paths1 = paths g1 in
  let paths2 = paths g2 in
  let product = BatList.cartesian_product paths1 paths2 in
    List.fold_left (fun g (e1, e2) ->
      let x1, f1, y1 = unpack e1 in
      let x2, f2, y2 = unpack e2 in
      if Field.equal f1 f2 then
        let g = G.add_edge_e g (x1, Disjoint (f1, x2), y1) in
        G.add_edge_e g (x2, Disjoint (f1, x1), y2)
      else g
    ) g product

let disjoint_union graphs =
  List.fold_left
    (fun acc g ->
      let must_allocs1 = must_alloc g in
      let must_allocs2 = must_alloc acc in
      let disequalities = BatList.cartesian_product must_allocs1 must_allocs2 in
      let union = update acc g in
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
  | SSL.PointsTo (SSL.Var x, SSL.Node.LS_t n) ->
    G.add_edge_e G.empty (x, Pointer Next, n)

  | SSL.PointsTo (SSL.Var x, SSL.Node.DLS_t (n, p)) ->
    let g = G.add_edge_e G.empty (x, Pointer Next, n) in
    G.add_edge_e g (x, Pointer Prev, p)

  | SSL.PointsTo (SSL.Var x, SSL.Node.NLS_t (t, n)) ->
    let g = G.add_edge_e G.empty (x, Pointer Next, n) in
    G.add_edge_e g (x, Pointer Top, t)

  | SSL.LS (SSL.Var x, SSL.Var y) ->
    if not @@ SSL.Variable.equal x y
    then G.add_edge_e G.empty (x, Path Next, y)
    else G.empty

  | SSL.DLS (SSL.Var x, SSL.Var y, SSL.Var px, SSL.Var ny) ->
    let g = G.add_edge_e G.empty (x, Path Next, y) in
    G.add_edge_e g (y, Path Prev, x)

  | SSL.NLS (SSL.Var x, SSL.Var y, SSL.Var z) ->
    if not @@ SSL.Variable.equal x y
    then
      let g = G.add_edge_e G.empty (x, Path Next, z) in
      G.add_edge_e g (x, Path Top, y)
    else G.empty

  | SSL.Star psis -> disjoint_union (List.map compute psis)
  | SSL.And (psi1, psi2) -> G.union (compute psi1) (compute psi2)
  | SSL.Or (psi1, psi2) -> G.intersect (compute psi1) (compute psi2)
  | SSL.GuardedNeg (psi1, psi2) -> compute psi1

  | SSL.Septraction _ -> G.empty
  | SSL.Not _ -> G.empty
  | SSL.Exists _ | SSL.Forall _ -> G.empty (* TODO *)

include G
