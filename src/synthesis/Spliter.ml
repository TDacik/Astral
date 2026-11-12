(* Spliting of formulae to atomic blocks used by synthesis.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

module Logger = Logging.WithOutput (struct
    let name = "Spliter"
    let level = 1
    let path = `Query "split_graphs"
  end)

let get_root phi = match SL.view phi with
  | PointsTo (x, _, _) -> Option.some x
  | Predicate (_, x :: _, _, _) -> Option.some x
  | _ -> None

let get_relevant_vars phi = match SL.view phi with
  | PointsTo (x, _, _) -> [x]
  | Predicate (_, x :: _, _, _) -> [x]
  | Eq xs -> xs
  | _ -> []

(** Ordering on terms *)

module OrderGraph = struct
  module G = Graph.Persistent.Digraph.ConcreteBidirectional(SL.Term)
  include G
  include Graph.Oper.P(G)
end

let is_lesser_in_atom t1 t2 atom = match SL.view atom with
  | PointsTo (root, _, ys)
  | Predicate (_, root :: ys, _, _) ->
    SL.Term.equal root t1 && SL.Term.MonoList.mem t2 ys
  | _ -> false

let compute_order_graph vars atoms =
  List_utils.diagonal_product vars
  |> List.fold_left (fun acc (t1, t2) ->
       if List.exists (is_lesser_in_atom t1 t2) atoms
       then OrderGraph.add_edge acc t1 t2
       else acc
     ) OrderGraph.empty
  |> OrderGraph.transitive_closure

let is_lesser_or_eq g t1 t2 =
  let open OrderGraph in
  if mem_edge g t1 t2 && not @@ mem_edge g t2 t1 then -1
  else if mem_edge g t2 t1 && not @@ mem_edge g t1 t2 then 1
  else SL.Term.compare t1 t2

module SplitGraph = struct

  module Label = struct
    include Bool
    let default = false
  end

  (** Graph on atomic formulae *)
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(SL)(Label)
  include G
  include Graph.Gmap.Edge(G)(struct include G let empty () = G.empty end)

  include Graph.Graphviz.Dot
    (struct
      include G

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_name v = Format.asprintf "\"%s\"" (SL.show v)
      let vertex_attributes v = []

      let get_subgraph _ = None
      let edge_attributes (_, is_split, _) =
        [`Style (if is_split then `Dashed else `Solid)]
      let default_edge_attributes _ = []
    end)

  module SplitView = struct
    type t = G.t
    module V = G.V
    let iter_vertex fn g = G.iter_vertex fn g
    let iter_edges fn g =
      G.iter_edges_e (fun (src, is_split, dst) ->
        if not is_split then fn src dst else ()
      ) g
  end

  module Components = Graph.Components.Undirected(SplitView)

  let compute_edge g existentials (src, _, dst) =
    let is_split =
      (not @@ SL.is_predicate src)
      && List.for_all (fun s -> not @@ SL.Term.is_heap_term (Option.get @@ get_root s)) (G.succ g src)
    in
    (src, is_split, dst)
    (*
    let src = get_root src in
    let dst = get_root dst in
    match src, dst with
    | Some src, Some dst ->
      not @@ (SL.Variable.MonoList.mem src existentials
      || SL.Variable.MonoList.mem dst existentials)
    | _ -> assert false*)

  let add_edge existentials g u v = G.add_edge g u v

  let compute phi =
    let es, atoms = SL.as_quantified_symbolic_heap phi in
    let add_edge = add_edge es in
    let g_base = List.fold_left (fun g atom -> match SL.view atom with
      | Emp -> g (* TODO: prevent learning ls(x,x) *)
      | PointsTo (_, _, ys) ->
        let successors = List.concat_map (SL.find_atoms_by_root phi) ys in
        let g0 = G.add_vertex g atom in
        List.fold_left (fun acc s -> add_edge acc atom s) g0 successors
      | Eq xs ->
        let worklist = List_utils.diagonal_product xs in
        List.fold_left (fun acc (u, v) ->
          let srcs = SL.find_atoms_by_root phi u in
          let dsts = SL.find_atoms_by_root phi v in
          BatList.cartesian_product srcs dsts
          |> List.fold_left (fun acc (src, dst) -> add_edge acc src dst) acc
        ) g worklist
      | Distinct _ -> g
      | Predicate (_, xs, _, _) ->
        (* TODO: could be refined *)
        let worklist = List_utils.diagonal_product xs in
        List.fold_left (fun acc (u, v) ->
          let srcs = SL.find_atoms_by_root phi u in
          let dsts = SL.find_atoms_by_root phi v in
          BatList.cartesian_product srcs dsts
          |> List.fold_left (fun acc (src, dst) -> add_edge acc src dst) acc
        ) g worklist

      | _ -> failwith "TODO"
    ) G.empty atoms
    in
    let res = map (compute_edge g_base es) g_base in
    G.fold_vertex (fun v acc -> G.add_vertex acc v) g_base res

  let get_root g =
    G.fold_vertex (fun v acc ->
      if G.in_degree g v == 0
      then v :: acc
      else acc
    ) g []

  let get_components phi g : SL.t list list =
    let _, atoms = SL.as_quantified_symbolic_heap phi in
    let pure = List.filter SL.is_pure atoms in
    match Components.components_list g with
    | [] -> [pure]
    | cs -> List.map (fun c -> c @ pure) cs

end

let get_vars_in_component c = List.concat_map get_relevant_vars c

let find_c_by_roots vars (example : SL.t list list) =
  List.find_opt (fun c -> not @@ SL.Term.MonoList.are_disjoint vars @@ get_vars_in_component c) example

(** The group function takes list of components for each example and creates
    several abstraction queries based on this decomposition. The queries are
    created as follows:

    1. The roots of all components in all examples are collected.
    2. The components are grouped by their root, if some example
       does not allocate a root, its pure part is used instead. *)

(** By the progress property, there should be unique root for each non
    pure component. *)
let get_component_root component =
  let g = SplitGraph.compute @@ SL.mk_star component in
  match SplitGraph.get_root g with
  | [] -> [] (* Pure component *)
  | [phi] -> [Option.get @@ get_root phi]
  | _ -> assert false

let has_root cmp root component =
  match get_component_root component with
  | [] -> false
  | [r] ->
    let g = SL_graph.compute @@ SL.mk_star component in
    let eqs = SL_graph.equivalence_class g r in
    SL.Term.equal root (snd @@ BatList.min_max ~cmp eqs)

let get_component_with_root cmp components root =
  List.map (fun c ->
    match List.find_opt (has_root cmp root) c with
    | Some res -> SL.mk_star res
    | None -> SL.mk_star @@ List.filter SL.is_pure @@ List.hd c
  ) components


let group vars (components : SL.t list list list) : (SL.Term.t * SL.t list) list =
  let vars =
    List.concat @@ List.concat @@ components
    |> SL.mk_star
    |> SL.free_vars
    |> List.map SL.Term.of_var
  in
  let og = compute_order_graph vars @@ List.concat @@ List.concat components in
  let cmp = is_lesser_or_eq og in
  let roots =
    List.concat components
    |> List.concat_map get_component_root
    |> SL.Term.MonoList.unique
  in
  Format.printf "Roots used to group components: %a\n" SL.Term.pp_list roots;
  List.map (fun root -> root, get_component_with_root cmp components root) roots


let matching_component vars other =
  match find_c_by_roots vars other with
  | Some c -> SL.mk_star c
  | None -> SL.mk_star @@ List.filter SL.is_pure @@ List.hd other

(** On the input, we have a list of components for each input example.
    We want to group together the components with the same root from all examples.

    If a component for some root does not exist, the pure context is used instead.
let group vars (components : SL.t list list list) =
  let vars = List.map SL.Term.of_var vars in
  let group_by main others =
    List.map (fun component ->
      let vars = SL.Term.MonoList.inter vars @@ get_vars_in_component component in
      SL.mk_star component :: List.map (matching_component vars) others
    ) main
  in
  match components with
  | [] -> []
  | [e] -> [List.map SL.mk_star e]
  | main :: others -> group_by main others
*)

let group_disequalities roots =
  let sorts = Sort.MonoList.unique @@ List.map SL.Term.get_sort roots in
  List.map (fun sort ->
    let xs = List.filter (SL.Term.has_sort sort) roots in
    SL.mk_distinct xs
  ) sorts

let add_implicit_disequalities phi =
  let es, atoms = SL.as_quantified_symbolic_heap phi in
  let roots =
    (*List.filter SL.is_pointer atoms
    |>*)
    List.filter_map get_root atoms
  in
  let disequalities = group_disequalities roots in
  SL.mk_exists es @@ SL.mk_star (disequalities @ atoms)

let compute_frame (examples : SL.t list) = match examples with
  | [] | [_] -> ([], examples)
  | _ ->
    let examples = List.map (fun e -> let g = SL_graph.compute ~predicates:false e in QuantifierElimination.apply g e) examples in
    let es, atoms = List.split @@ List.map (fun e -> SL.as_quantified_symbolic_heap e) examples in
    let frame, rests = List_utils.split_common SL.compare SL.is_spatial_atom atoms in
    let rests = List.map2 (fun e atoms -> SL.mk_exists e @@ SL.mk_star atoms) es rests in
    (frame, rests)

let split (examples : SL.t list) vars : (SL.t list * (SL.Term.t * SL.t list) list) =
  let examples = List.map add_implicit_disequalities examples in
  let frame, examples = compute_frame examples in
  Format.printf "Before split:\n";
  List.iteri (fun i phi ->
    Format.printf "  example %d: %s\n" (i+1) (SL_printer.pretty phi);
  ) examples;
  let components =
    List.mapi (fun i e ->
      let g = SplitGraph.compute e in
      Logger.generic (Format.asprintf "example_%d.dot" (i+1)) SplitGraph.output_graph g;
      SplitGraph.get_components e g) examples
  in
  List.iteri (fun i cs ->
    Format.printf "Example %d:\n" (i+1);
    List.iteri (fun i c ->
      Format.printf "  Component %d:\n  - %s\n" (i+1) (SL_printer.pretty_list ~sep:"\n  - " c)
    ) cs
  ) components;
  let formulae = group vars components in
  let formulae =
    List.map (fun (root, components) ->
      let cs = List.map (fun c ->
        let all_vars = SL.free_vars c in
        let existentials = SL.Variable.MonoList.diff all_vars (SL.Variable.nil :: vars) in
        SL.mk_exists existentials c
      ) components
      in
      root, cs) formulae
  (*
      (List.map (fun f ->
      let all_vars = SL.free_vars f in
      let existentials = SL.Variable.MonoList.diff all_vars (SL.Variable.nil :: vars) in
      r, SL.mk_exists existentials f
    ) top)) formulae*)
  in
  List.iteri (fun i (r, fs) ->
    Format.printf "Query %d for root %a:\n" (i+1) SL.Term.pp r;
    List.iter (fun f -> Format.printf " - %s\n" (SL_printer.pretty f)) fs) formulae;
  frame, formulae
