(* Representation of SL graph
 *
 * This module is factored out to prevent cyclic dependency between InductiveDefinition and
 * SL graph module.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2024 *)

open MemoryModel

module SL_edge = struct

  type t =
    | No
    | Equality
    | Disequality
    | Pointer of Field.t
    | Path of Field.t
    | Disjoint of Field.t * SL.Term.t
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

  let show = function
    | No            -> assert false
    | Pointer field -> "→" ^ Field.show field
    | Path field    -> "⇝" ^ Field.show field
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
    (SL.Term)
    (SL_edge)

  include Self
  include Graph.Oper.P(Self)

  (* Redefine add_edge to do not include equality self-loops.

     Be very careful, this could break small model computation! *)

  let add_edge_e g (src, label, dst) =
    if SL.Term.equal src dst && SL_edge.equal label SL_edge.Equality then g
    else add_edge_e g (src, label, dst)

  include Graph.Graphviz.Dot
    (struct
      include Self

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_name v = Format.asprintf "\"%s\"" (SL.Term.show v)
      let vertex_attributes v = []

      let get_subgraph _ = None
      let edge_attributes e = [
        `Label (SL_edge.show @@ E.label e);
        `Arrowhead (match E.label e with | Pointer _ | Path _ -> `Normal | _ -> `None);
        `Style (match E.label e with | Pointer _ | Path _ -> `Solid | _ -> `Dashed);

      ]
      let default_edge_attributes _ = []
    end)

  let output_file path g =
    let channel = open_out path in
    output_graph channel g;
    close_out channel

end

let compare_edge_e e1 e2 =
  let src1, label1, dst1 = e1 in
  let src2, label2, dst2 = e2 in
  let aux = SL_edge.compare label1 label2 in
  if aux <> 0 then aux
  else match label1 with
    | Equality | Disequality ->
      let aux1 = SL.Term.equal src1 dst1 && SL.Term.equal src2 dst2 in
      let aux2 = SL.Term.equal src2 dst1 && SL.Term.equal src1 dst2 in
      if aux1 || aux2 then 0 else G.E.compare e1 e2
    | Pointer _ -> G.E.compare e1 e2
    | _ -> assert false (* Used only for SL-graphs *)

let get_vertices g = G.fold_vertex List.cons g []
let get_edges g = G.fold_edges_e List.cons g []

let get_fields g =
  get_edges g
  |> List.map G.E.label
  |> List.filter SL_edge.is_spatial_any
  |> List.map SL_edge.get_field

let compare g1 g2 = List.compare compare_edge_e (get_edges g1) (get_edges g2)

let equal g1 g2 = compare g1 g2 = 0

let show g =
    if G.is_empty g then "empty"
    else
    G.fold_edges_e (fun (x, label, y) acc ->
      Format.asprintf " %s  - %s (%s) %s\n" acc (SL.Term.show x) (SL_edge.show label) (SL.Term.show y)
    ) g ""

module S = struct
  type nonrec t = G.t
  let compare = compare
  let show = show
end

include Datatype.Printable(S)
include Datatype.Collections(S)

(** Projections *)

let filter g pred =
  G.fold_edges_e (fun e g -> if pred e then G.add_edge_e g e else g) g G.empty

let projection g labels = filter g (fun (_, label, _) -> List.mem label labels)

let projection_sort sort g = filter g (fun (x, _, _) -> SL.Term.has_sort sort x)

let pure_projection g = projection g [Equality; Disequality]

let projection_eq g =
  let proj = projection g [Equality] in
  let proj_mirror = G.mirror proj in
  G.union proj proj_mirror
  |> G.transitive_closure ~reflexive:true

let projection_neq g = projection g [Disequality]
let projection_pointer g = filter g (fun (_, label, _) -> SL_edge.is_pointer label)
let spatial_projection g = filter g (fun (_, label, _) -> SL_edge.is_spatial_any label)
let projection_field field g = projection g [Equality; Disequality; Pointer field; Path field]


let projection_path g = filter g (fun (_, label, _) -> SL_edge.is_path label)
let projection_path_field field g = projection g [Equality; Pointer field; Path field]

let neighbours g x =
  let preds = try G.pred g x with Invalid_argument _ -> [] in
  let succs = try G.succ g x with Invalid_argument _ -> [] in
  preds @ succs

let equivalence_class g x =
  let g = projection g [Equality] in
  BatList.unique ~eq:G.V.equal (x :: neighbours g x)

let must_disjoint_with g x field y =
  G.fold_edges_e (fun e acc -> match e with
    | (x', Disjoint (field', ell), y') when
      SL.Term.equal x x' && SL.Term.equal y y' && Field.equal field field' ->
      ell :: acc
    | _ -> acc
  ) g []

let must_disjoint g field x y ell =
  BatList.mem_cmp G.V.compare ell (must_disjoint_with g x field y)

let must_eq g x y =
  let g = projection_eq g in
  G.mem_edge g x y || SL.Term.equal x y

(* TODO: improve using eq_classes? *)
let must_neq g x y =
  let g = projection_neq g in
  G.mem_edge g x y || G.mem_edge g y x

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

let rec must_allocated ?(visited=[]) v g =
  if BatList.mem_cmp SL.Term.compare v visited then false
  else
    must_pointer_any g v
    || must_proper_path_any g v
    || (List.exists (fun x -> must_allocated x g ~visited:(v::visited)) @@ equivalence_class g v)

let must_alloc g =
  let vs = G.fold_vertex List.cons g [] in
  List.filter (fun v -> must_allocated v g) vs

let nb_allocated ?(distinct=false) g =
  let must_eq_cmp v1 v2 =
    if must_eq g v1 v2 then 0
    else G.V.compare v1 v2
  in
  let allocated = must_alloc g in
  let filtered =
    if distinct then BatList.unique_cmp ~cmp:must_eq_cmp allocated
    else allocated
  in
  List.length filtered

let nb_roots g =
  G.fold_vertex List.cons g []
  |> List.filter (fun x -> must_root g x)
  |> List.length

(** TODO: SL-graph normalisation *)
let must_successor_ptr g field source =
  try
    let g = projection g [Pointer field] in
    Option.some @@ List.hd @@ G.succ g source
  with Invalid_argument _ | Failure _ -> None

let must_successor_any g field source =
  let g = projection g [Pointer field; Path field] in
  List.hd @@ G.succ g source

let must_successor_edge g field source =
  let g = projection g [Pointer field; Path field] in
  List.hd @@ G.succ_e g source

let nb_joins g field =
  let g = projection g [Pointer field] in
  G.fold_vertex List.cons g []
  |> List.map (fun v -> if G.in_degree g v > 1 then G.in_degree g v - 1 else 0)
  |> BatList.sum

include G

let must_pred_field g x =
  let g = projection_pointer g in
  try
    let preds = G.pred_e g x in
    let e = List.hd preds in
    let src = G.E.src e in
    let field = SL_edge.get_field @@ G.E.label e in
    Some (src, field)
  with _ -> None

module CC = Graph.Components.Undirected(G)

let is_connected g =
  let spatial_g = spatial_projection g in
  print spatial_g;
  let components = CC.components_list spatial_g in
  List.iter SL.Term.print_list components;
  List.length components == 1

let are_skeleton_fields g fields =
  assert (is_connected g);
  let projection =
    filter g (fun (_, label, _) ->
      SL_edge.is_spatial_any label && BatList.mem_cmp Field.compare (SL_edge.get_field label) fields
    )
  in
  is_connected projection



(** ==== Evaluation ==== *)

let rec eval_term g t =
  match SL.Term.view t with
    | Var _ -> Some t
    | HeapTerm (f, s) ->
      match eval_term g s with
      | None -> None
      | Some s -> must_successor_ptr g f s

let lift g eval_true eval_false x y =
  let open ThreeValuedLogic in
  let x, y = eval_term g x, eval_term g y in
  if Option.is_none x || Option.is_none y then Unknown
  else
    let x, y = Option.get x, Option.get y in
    if eval_true g x y then True
    else if eval_false g x y then False
    else Unknown

let eval_predicate g c = match SL.view c with
  | SL.Eq [x; y] -> lift g must_eq must_neq x y
  | SL.Distinct [x; y] -> lift g must_neq must_eq x y
  | _ -> failwith "TODO"
