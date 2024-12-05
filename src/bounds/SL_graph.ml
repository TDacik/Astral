(* Operations over SL graph
 *
 * The representation of SL graph is in the module SL_graph0
 *
 * TODO: Sl-graph normalisation
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open MemoryModel

include SL_graph0
open SL_edge

(** ==== Vertex substitution ==== *)

  module GM = Graph.Gmap.Vertex(G)(struct include G let empty () = G.empty end)

  let substitute g ~vertex ~by =
    if SL.Term.equal vertex by then g
    else
      let g0 = G.add_vertex g by in
      let g1 =
        try
          G.fold_succ_e (fun (_, label, dst) acc ->
            G.add_edge_e acc (by, label, dst)) g0 vertex g0
        with Invalid_argument _ -> g0
      in
      let g2 =
        try
          G.fold_pred_e (fun (src, label, _) acc ->
            G.add_edge_e acc (src, label, by)) g1 vertex g1
        with Invalid_argument _ -> g1
      in
      let g3 = G.remove_vertex g2 vertex in
      g3

  (*
  GM.map (fun v -> if G.V.equal v vertex then by else v) g
  *)

  let substitute_list g ~vertices ~by =
    BatList.fold_left2 (fun g vertex by -> substitute g ~vertex ~by) g vertices by

(** ==== SL-graph construction ==== *)

let all_equal xs =
  List_utils.diagonal_product xs
  |> List.map (function (x, y) -> (x, Equality, y))
  |> List.fold_left G.add_edge_e G.empty

let all_distinct xs =
  List_utils.diagonal_product xs
  |> List.map (function (x, y) -> (x, Disequality, y))
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

let disjoint_union ?(stars=true) graphs =
  List.fold_left
    (fun acc g ->
      let must_allocs1 = must_alloc g in
      let must_allocs2 = must_alloc acc in
      let disequalities = BatList.cartesian_product must_allocs1 must_allocs2 in
      let union = if stars then update acc g else G.union acc g in
      List.fold_left
        (fun g (x, y) ->
        G.add_edge_e g (x, Disequality, y)
      ) union disequalities
    ) G.empty graphs

let of_pointer x struct_def ys =
  let fields = StructDef.get_fields struct_def in
  List.fold_left2 (fun g field y ->
    G.add_edge_e g (x, Pointer field, y)
  ) G.empty fields ys

let rec compute stars phi =
  let compute = compute stars in
  match SL.view phi with
  | SL.Emp | SL.Pure _ | SL.True | SL.False -> G.empty
  | SL.Eq xs -> all_equal xs
  | SL.Distinct xs -> all_distinct xs
  | SL.PointsTo (x, s, ys) -> of_pointer x s ys
  | SL.Predicate (id, xs, defs) -> SID.sl_graph id (xs, defs)

  | SL.Star psis -> disjoint_union ~stars (List.map compute psis)
  | SL.And psis -> List.fold_left G.union G.empty (List.map compute psis)
  | SL.Or psis -> List.fold_left G.intersect G.empty (List.map compute psis)
  | SL.GuardedNeg (psi1, psi2) -> compute psi1

  | SL.Septraction _ -> G.empty
  | SL.Not _ -> G.empty

  (** TODO: more precise? *)
  | SL.Ite (_, psi_then, psi_else) -> G.union (compute psi_then) (compute psi_else)


  (** TODO *)
  | SL.Exists (xs, psi) | SL.Forall (xs, psi) -> compute psi

let normalise g =
  let nil = SL.nil in
  let alloc = must_alloc g in
  let g =
    List.fold_left (fun g x -> G.add_edge_e g (x, Disequality, SL.Term.nil)) g alloc
  in
  let g_ptr = projection_pointer g in
  G.fold_edges_e (fun e acc ->
    G.fold_edges_e (fun e' acc ->
      let x, l, y = e in
      let x', l', y' = e' in
      if must_eq g x x' && SL_edge.is_pointer l && SL_edge.equal l l' then G.add_edge_e acc (y, Equality, y') (* TODO: on which graph should \musteq be checked? *)
      else acc
    ) g_ptr acc
  ) g_ptr g

exception Contradiction

let has_contradiction g =
  try
    G.iter_vertex (fun x ->
      G.iter_vertex (fun x' ->
        if must_eq g x x' && must_neq g x x' then
        raise Contradiction
      ) g
    ) g;
    false
  with Contradiction -> true

let do_normalise = normalise

let compute ?(normalise=true) ?(stars=true) phi =
  let g = compute stars phi in
  if normalise then do_normalise g else g
