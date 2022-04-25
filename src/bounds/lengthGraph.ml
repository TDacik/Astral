(* Length-graph
 *
 * TODO: replace with SL-graph?
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open LengthConstraints

open Graph

module G_aux = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
  (SSL.Variable)
  (struct
    include AbstractLength
    let default = top
  end)

module Graph = struct

  module G = struct
    include G_aux
    include Oper.P(G_aux)

  end

  (* Return default label for non-existing edge *)
  let find_edge g x y =
    try G.find_edge g x y
    with Not_found -> (x, AbstractLength.top, y)

  let get_length g x y = G.E.label @@ find_edge g x y

  let set g x y length =
    let current = find_edge g x y in
    let g = G.remove_edge_e g current in
    G.add_edge_e g (x, AbstractLength.of_list length, y)

  let remove g x y length =
    let current = find_edge g x y in
    let updated = AbstractLength.remove length (G.E.label current) in
    let g =
      try G.remove_edge_e g current
      with _ -> g
    in
    G.add_edge_e g (x, updated, y)

  let remove_list g x y lengths =
    List.fold_left (fun g l -> remove g x y l) g lengths

  (** Build the most coarse length abstraction *)
  let top phi = G.empty

  let single_edge x y label_list =
    let set = AbstractLength.of_list label_list in
    G.add_edge_e G.empty (x, set, y)

  let update_edge g (x, label1, y) fn =
    try
      let _, label2, _ = G.find_edge g x y in
      let new_label = fn label1 label2 in
      let g = G.remove_edge g x y in
      G.add_edge_e g (x, new_label, y)
    with Not_found -> G.add_edge_e g (x, label1, y)

  let update_edge_union g e = update_edge g e AbstractLength.union

  let update_edge_inter g e = update_edge g e AbstractLength.inter

  let union g1 g2 = G.fold_edges_e (fun e g -> update_edge_union g e) g1 g2

  let inter g1 g2 = G.fold_edges_e (fun e g -> update_edge_inter g e) g1 g2

  let must_eq g x y = AbstractLength.must_eq @@ get_length g x y

  let must_neq g x y = AbstractLength.must_neq @@ get_length g x y

  let neq_sym g =
    G.fold_edges_e
      (fun (x, ls, y) g ->
        if must_neq g x y
        then remove (remove g y x Equal) y x EqualCircular
        else g
      ) g g

  let eq_sym g =
    G.fold_edges_e
      (fun (x, ls, y) g ->
        if AbstractLength.is_singleton Equal ls
        then set g y x [Equal]
        else g
      ) g g

  let reflexivity g =
    G.fold_vertex (fun x g -> remove_list g x x [Infinity; Pointer; List]) g g

  let nil g = remove_list g Nil Nil [Infinity; EqualCircular; Pointer; List]

  let reach_trans_iter g =
    G.fold_edges_e
      (fun (x, l1, y) g ->
        G.fold_succ_e
          (fun (_, l2, z) g ->
            if AbstractLength.must_list l1 && AbstractLength.must_list l2
            then remove g x z Infinity
            else g
          ) g y g
      ) g g

  let fixp_equal g1 g2 =
    let s1 = G.fold_edges_e (fun (_, l, _) acc -> acc + AbstractLength.cardinal l) g1 0 in
    let s2 = G.fold_edges_e (fun (_, l, _) acc -> acc + AbstractLength.cardinal l) g2 0 in
    Int.equal s1 s2

  let rec reach_trans g =
    let g' = reach_trans_iter g in
    if fixp_equal g g'
    then g
    else reach_trans g'

  let eq_circular_remove g =
    G.fold_edges_e
      (fun (x, l1, y) g ->
        G.fold_succ_e
          (fun (_, _, z) g ->
            if AbstractLength.must_non_empty_list l1
               || AbstractLength.is_singleton Pointer l1
            then remove g x x EqualCircular
            else g
          ) g y g
      ) g g

  let closure g =
    reach_trans (eq_sym (neq_sym g))
    |> reflexivity
    |> nil
    |> eq_circular_remove

  let must_nonempty g x y = AbstractLength.must_non_empty_list (G.E.label @@ find_edge g x y)

  let must_pointer g x y = AbstractLength.must_pointer (G.E.label @@ find_edge g x y)

  let pointer_projection g =
    G.fold_edges_e
      (fun (x, l, y) acc ->
        if must_pointer g x y
        then G.add_edge_e acc (x, l, y)
        else acc
      ) g G.empty

  (** True if vertex has surely have a named succesor *)
  let named_succ g x =
    let labels = G.fold_succ_e (fun e acc -> G.E.label e :: acc) g x [] in
    List.exists AbstractLength.must_pointer labels

  let nb_must_pointers g =
    G.fold_vertex (fun v acc -> v :: acc) g []
    |> List.filter (named_succ g)
    |> List.length

  module Dijkstra = Path.Dijkstra
    (G)
    (struct
      type edge = G.E.t
      type t = Int.t
      let weight _ = 1
      let compare = Int.compare
      let add = (+)
      let zero = 0
    end)

  let must_path g x y max =
    let must = pointer_projection g in
    try snd @@ Dijkstra.shortest_path must x y
    (* No path, or vertices are not in must graph *)
    with _ -> max

  let get_contradiction g =
    let edges = G.fold_edges_e List.cons g [] in
    try
      let x, _, y = List.find (fun (x, l, y) -> AbstractLength.is_contradiction l) edges in
      Some (x,y)
    with Not_found -> None

  let is_allocated g x =
    let labels = G.fold_succ_e (fun e acc -> G.E.label e :: acc) g x [] in
    List.exists AbstractLength.must_allocated labels

  let nb_allocated g =
    G.fold_vertex (fun v acc -> v :: acc) g []
    |> List.filter (is_allocated g)
    |> List.length

  let nb_must_preds_v g x =
    G.fold_vertex (fun v acc -> v :: acc) g []
    |> List.filter (fun v -> must_pointer g v x)
    |> List.length
    |> (fun x -> max (x-1) 0)

  let nb_must_forks g =
    G.fold_vertex (fun v acc -> v :: acc) g []
    |> List.map (fun v -> nb_must_preds_v g v)
    |> BatList.sum

  let inclusion g1 g2 =
    G.fold_edges_e
      (fun (x, l1, y) acc ->
        let _, l2, _ = find_edge g2 x y in
        acc && AbstractLength.subset l1 l2
      ) g1 true

  let g_ref = ref None

  module Dot = Graph.Graphviz.Dot
    (struct
      include G

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_name = SSL.Variable.show
      let vertex_attributes v =
        [ `Style `Filled;
          `Fillcolor (if is_allocated (Option.get !g_ref) v then 1556715 else 16777215)]
      let get_subgraph _ = None
      let edge_attributes e = [`Label (AbstractLength.show @@ E.label e)]
      let default_edge_attributes _ = []
    end)

  let output_graph channel g =
    g_ref := Some g;
    Dot.output_graph channel g

end

include Graph.G
include Graph

let output_file g path =
  let channel = open_out path in
  Graph.output_graph channel g;
  close_out channel

let rec _compute phi = match phi with
  | Eq (x, y) -> Graph.single_edge x y [Equal; EqualCircular]
  | Neq (x, y) -> Graph.single_edge x y [Infinity; Pointer; List]
  | PointsTo (x, y) -> Graph.single_edge x y [Pointer; EqualCircular]
  | LS (x, y) -> Graph.single_edge x y [Equal; Pointer; List]

  | Star (psi1, psi2) -> Graph.inter (_compute psi1) (_compute psi2)
  | And (psi1, psi2) -> Graph.inter (_compute psi1) (_compute psi2)
  | Or (psi1, psi2) -> Graph.union (_compute psi1) (_compute psi2)
  | GuardedNeg (psi1, psi2) -> _compute psi1

  | Septraction _ -> Graph.G.empty
  | Not _ -> Graph.G.empty

let compute phi =
  let g = _compute phi in
  Graph.closure g

let get_contradiction phi =
  let g = compute phi in
  Graph.get_contradiction g
