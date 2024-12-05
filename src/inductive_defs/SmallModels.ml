(* Implementation of search of small models of inductive defintions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open InductivePredicate

module Logger = Logger.MakeWithDir
  (struct let dirname = "small-models" let name = "small-models" let level = 1 end)

module Abstraction = struct

  let is_existential term : bool =
    let name = SL.Term.show term in
    String.contains name '!'

  let remove_existentials g =
    SL_graph.fold_vertex (fun v acc ->
      if not @@ is_existential v then acc
      else SL_graph.remove_vertex acc v
    ) g g

  let nb_alloc_existentials g =
    SL_graph.fold_vertex (fun v acc ->
      if is_existential v then acc + 1
      else acc
    ) g 0

  module Set = Stdlib.Set.Make(struct
    type t = SL_graph.t
    let compare g1 g2 = Stdlib.compare g1 g2
    (*
      let bool1 = SL_graph.pure_projection @@ remove_existentials g1 in
      let bool2 = SL_graph.pure_projection @@ remove_existentials g2 in
      if SL_graph.compare bool1 bool2 <> 0 then SL_graph.compare bool1 bool2
      else
        let n1 = nb_alloc_existentials g1 in
        let n2 = nb_alloc_existentials g2 in
        if n2 > n1 && (n1 > 1 || n2 > 1) then 1
        else if n1 > n2 && (n2 > 1 || n1 > 1) then -1
        else if (n2 > 1 || n1 > 1) then 0
        else Stdlib.compare g1 g2
    *)
  end)

  type t = Set.t

  let mk gs = Set.of_list gs

  let join a1 a2 = Set.union a1 a2

  let to_list s = Set.elements s

  (*
  let join_list xs = List.fold_left join xs []
  *)

  let cnt = ref 0

  let equal a1 a2 =
    Set.cardinal a1 > 2 && Set.cardinal a2 > 2


end

(** Graph based *)

let get_atoms phi =
  SL.map_view (function
    | Predicate _ -> SL.emp
  ) phi
  (*
  | Star (psis) when List.for_all SL.is_atom psis -> List.split SL.is_predicate
  | _ -> raise @@ SolverUtils.UnsupportedFragment "Complex inductive predicate"
  *)

let unfold_one pred case =
  SL.map_view (function
    | Predicate (name, xs, _) when name = pred.name ->
      InductivePredicate.instantiate ~refresh:false pred xs
  ) case

(** Initial states are computed by taking bases cases predicates. If the predicate has no
    base cases, its initial state is the empty list *)
let initial_states predicate =
  predicate.base_cases
  |> List.map SL_graph.compute
  |> Abstraction.mk

(** Fixpoint step *)

let get_preds parent case =
  SL.select_subformulae (fun psi -> match SL.view psi with
    | SL.Predicate (name, xs, _) when String.equal name parent.name -> true
    | _ -> false
  ) case
  |> List.map (fun x -> match SL.view x with Predicate (_, xs,  _) -> xs)

let cnt = ref 0

let compute_pred pred data call =
  let vertices = List.map SL.Term.of_var pred.header in
  List.map (fun g ->
    let g' = SL_graph.substitute_list g ~vertices ~by:call in
    cnt := !cnt + 1;
    (*Logger.dump SL_graph.G.output_file (Format.asprintf "g_before%d.dot" !cnt) g;
    Logger.dump SL_graph.G.output_file (Format.asprintf "g_after%d.dot" !cnt) g';
    *)g'
    ) (Abstraction.to_list data)

let compute_case parent data case =
  let atoms = get_atoms case in
  let predicates = get_preds parent case in
  let base_sl_graph = SL_graph.compute ~normalise:false ~stars:false atoms in
  let predicate_graphs = List.concat_map (compute_pred parent data) predicates in
  List.map (fun g -> SL_graph.disjoint_union ~stars:false [g; base_sl_graph]) predicate_graphs
(*
let find_pairs psi = SL.select_subformulae (fun psi -> match SL.view psi with
  | PointsTo (x, def, ys) -> true
  | _ -> false
  ) psi
  |> List.concat_map (function PointsTo (x, def, ys) -> List.map  ys

let qelim symbolic_heap =
  let pairs = find_pairs symbolic_heap in
  let vars, by = List.map (fun source, field -> source, SL.Term.mk_heap_term source field) pairs
  SL.substitute symbolic_heap ~vars ~by
*)
let compute_edge (parent, child) data : Abstraction.t =
  Logger.debug "Computing: %s -> %s\n" parent.name child.name;
  (InductivePredicate.refresh child).inductive_cases
  |> List.map (compute_case parent data)
  |> List.concat
  |> Abstraction.mk

module G = DependencyGraph

module Fixpoint = Graph.Fixpoint.Make(G)
  (struct
    type g = G.t
    type vertex = G.V.t
    type edge = G.E.t

    type data = Abstraction.t
    let direction = Graph.Fixpoint.Forward
    let equal = Abstraction.equal
    let join = Abstraction.join
    let analyze = compute_edge
  end)

let compute_fixpoint dependency_graph =
  Fixpoint.analyze initial_states dependency_graph

module M = InductivePredicate.Map

(* TODO: computation *)
let to_map g fn =
  G.fold_vertex (fun v acc ->
    let sum = BatList.max @@ List.map SL_graph.nb_allocated
              @@ Abstraction.to_list @@ fn v in
    M.add v 2 acc) g M.empty (* TODO: !!!!!! *)

let dump_small_models g get_models =
  Logger.debug "Results:\n";
  G.iter_vertex (fun v ->
    let models = Abstraction.to_list @@ get_models v in
    Logger.debug "  %s: %s\n" v.name (String.concat ", " @@ List.map (fun g -> string_of_int @@ SL_graph.nb_allocated g) models) ;
    List.iteri (fun i g -> Logger.dump SL_graph.G.output_file (Format.asprintf "%s%d.dot" v.name i) g) models
  ) g

[@@@ ocaml.warning "-5"]

let compute g =
  Logger.debug "Computing small models of predicates\n";
  dump_small_models g (compute_fixpoint g);
  to_map g @@ compute_fixpoint g
