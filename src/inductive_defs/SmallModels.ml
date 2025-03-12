(* Implementation of search of small models of inductive defintions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module G = DependencyGraph

module Logger = Logger.MakeWithDir
  (struct let dirname = "small-models" let name = "small-models" let level = 1 end)

module SL_graph = struct
  include SL_graph

  let is_existential term : bool =
    let name = SL.Term.show term in
    String.contains name '!' (* TODO: do this properly *)

  let has_existential g =
    SL_graph.fold_vertex List.cons g []
    |> List.exists (fun v ->
          is_existential v
          && List.for_all is_existential (SL_graph.equivalence_class g v)
       )

  let contract g = contract g is_existential

  let remove_existentials g =
   SL_graph.fold_vertex (fun v acc ->
      if not @@ is_existential v then acc
      else SL_graph.remove_vertex acc v
    ) g g

  let size g = Float.of_int @@ SL_graph.nb_allocated g
  (*
    Float.div
      (Float.of_int @@ SL_graph.nb_allocated g)
      Float.one
      (Float.of_int @@ SL_graph.nb_allocated @@ remove_existentials g)
   *)
end

(** Hyperedge represents an unprocessed predicate occurrence. *)
module Hyperedge = struct

  type t = InductiveDefinition.t * SL.Term.t List.t [@@deriving equal, compare]

  let show (id, xs) = Format.asprintf "%s(%s)" (InductiveDefinition.name id) (SL.Term.show_list xs)

  let of_formula phi = match SL.view phi with
    | SL.Predicate (name, xs, _) -> (SID.find_user_defined name, xs)
    | _ -> failwith @@ SL.show phi

  let match_id id self = Identifier.equal_with_string id (InductiveDefinition.name (fst self))

  (** Replace hyperedge by a list of its possible SL-formula instances. *)
  let unfold (pred, xs) = InductiveDefinition.cases pred ~refresh:true ~params:xs

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Collections(Self)

end


module Hypergraph = struct

  type t = {
    graph : SL_graph.t;
    hyper_edges: Hyperedge.Set.t;
  } [@@deriving equal, compare]

  let show self =
    Format.asprintf "Graph: %d\nHyperdges:\n  %s\n"
      (SL_graph.nb_allocated self.graph)
      (String.concat "\n  " @@ List.map Hyperedge.show @@ Hyperedge.Set.elements self.hyper_edges)

  let of_inductive_def id params = {
    graph = SL_graph.empty;
    hyper_edges = Hyperedge.Set.singleton @@ Hyperedge.of_formula @@ InductiveDefinition.to_formula id ~params;
  }

  (* TODO: fragile *)
  let rec formula_split phi =
    let psis = match SL.view phi with
      | Exists (_, psi) -> let lhs, rhs = formula_split psi in lhs @ rhs
      | Star psis -> List.concat_map (fun psi -> let lhs, rhs = formula_split psi in lhs @ rhs) psis
      | _ when SL.is_atom phi -> [phi]
      | And psis -> List.concat_map (fun psi -> let lhs, rhs = formula_split psi in lhs @ rhs) psis
      | _ -> failwith @@ SL.show phi
    in
    List.partition SL.is_predicate psis

  let of_formula phi =
    let predicates, atoms = formula_split phi in
    {
      graph = SL_graph.compute @@ SL.mk_star atoms;
      hyper_edges = Hyperedge.Set.of_list @@ List.map Hyperedge.of_formula predicates;
    }

  let disjoint_union hg1 hg2 = {
    graph = SL_graph.disjoint_union ~stars:false [hg1.graph; hg2.graph];
    hyper_edges = Hyperedge.Set.union hg1.hyper_edges hg2.hyper_edges;
  }

  let size self = SL_graph.nb_allocated self.graph

  let is_atomic self = Hyperedge.Set.is_empty self.hyper_edges

  (** Unfold all hypergraphs upto size n *)
  let unfold n self =
    Hyperedge.Set.fold (fun selected acc ->
      let rest = {self with hyper_edges = Hyperedge.Set.remove selected self.hyper_edges} in
      let unfoldings = Hyperedge.unfold selected in
      let hypergraphs = List.map of_formula unfoldings in
      let res = List.map (disjoint_union rest) hypergraphs in
      res @ acc
    ) self.hyper_edges []

end

module Derivation = struct
  open Hypergraph

  module HS = Set.Make(Hypergraph)
  module GS = Set.Make(SL_graph)

  type t = {
    graphs : GS.t;
    worklist : HS.t;
    finished : HS.t;
  }

  let empty = {
    graphs = GS.empty;
    worklist = HS.empty;
    finished = HS.empty;
  }

  let add_hypergraph self hg =
    if Hyperedge.Set.is_empty hg.hyper_edges then
      let g = SL_graph.contract hg.graph in
      {self with graphs = GS.add g self.graphs}
    else if not @@ HS.mem hg self.finished then
      {self with worklist = HS.add hg self.worklist}
    else self

  let initial id =
    let params = List.map SL.Term.of_var @@ InductiveDefinition.header id in
    add_hypergraph empty (Hypergraph.of_inductive_def id params)

  let leafs self = GS.elements self.graphs

  let worklist self = HS.elements self.worklist

  let unfold_aux n self =
    let self = {self with finished = HS.union self.worklist self.finished} in
    let worklist =
      worklist self
      |> List.filter (fun h -> Hypergraph.size h < n)
    in match worklist with
      | [] -> self
      | hgs ->
        List.concat_map (Hypergraph.unfold n) hgs
        |> BatList.unique ~eq:Hypergraph.equal
        |> List.fold_left add_hypergraph {self with worklist = HS.empty}

  let rec unfold n self =
    if HS.for_all (fun h -> Hypergraph.size h >= n) self.worklist then self
    else unfold n (unfold_aux n self)

  let get_pure der =
    leafs der
    |> List.map SL_graph.remove_existentials
    |> List.map SL_graph.pure_projection
    |> SL_graph.Set.of_list

  let is_stable der1 der2 =
    let pure1 = get_pure der1 in
    let pure2 = get_pure der2 in
    SL_graph.Set.equal pure1 pure2

  let negated phi id =
    let name = InductiveDefinition.name id in
    match SL.view phi with
    | _ when SL.is_negation_free phi -> []
    | GuardedNeg (_, rhs) when SL.is_symbolic_heap rhs ->
      let _, atoms = SL.as_quantified_symbolic_heap rhs in
      List.filter_map (fun atom -> match SL.view atom with
        | PointsTo _ -> Some atom
        | Predicate (name', _, _) when String.equal name name' -> Some atom
        | _ -> None
      ) atoms
    | _ -> failwith @@ SL.show phi

  let is_unique_ptr negated ders =
    let ptrs = List.filter SL.is_pointer negated in
    match ptrs with
      | [] -> true
      | _ -> List.exists SL_graph.has_existential ders

  let is_fixpoint n id phi der others =
    match negated phi id with
      | [] -> true
      | ns ->
        let ders = leafs der in(*
        List.exists (fun d -> SL_graph.nb_allocated d > n && not @@ BatList.exists (SL_graph.eq_iso d) others) ders
        &&*) is_unique_ptr ns ders

  let size der =
    leafs der
    |> List.map SL_graph.size
    |> BatList.max

  let cardinal self = HS.cardinal self.worklist

  let show ?(indent=0) self =
    GS.iter (fun g -> Logger.debug "%s" (SL_graph.show g)) self.graphs
    (*
    Format.printf "G: %d, W: %d, F: %d\n"
      (GS.cardinal self.graphs)
      (HS.cardinal self.worklist)
      (HS.cardinal self.finished);
    (*GS.iter (fun g -> Format.printf "g:\n  %d\n" (SL_graph.nb_allocated g)) self.graphs;*)
    HS.iter (fun h -> Format.printf "F:\n  %s\n" (Hypergraph.show h)) self.finished;
    HS.iter (fun h -> Format.printf "H:\n  %s\n" (Hypergraph.show h)) self.worklist
    *)

  let debug pred der = ()
                       (*
    Logger.debug "Derivation %s:\n" (InductiveDefinition.name pred);
    show der;
    leafs der
    |> List.iteri (fun i g ->
      let name = Format.asprintf "%s_%d" (InductiveDefinition.name pred) i in
      Logger.dump SL_graph.G.output_file (name ^ ".xdot") g;
      Logger.dump SL_graph.G.output_file (name ^ "_pure.xdot") (SL_graph.remove_existentials @@ SL_graph.pure_projection g);
    *)

end

(** Result of small-model computation:

    Mapping from inductive predicates to explored derivations. *)
module Result = struct

  module M = InductiveDefinition.Map

  type t = Derivation.t M.t

  type sizes = {
    lhs_unfolding : int;
    small_bound : float;
    bound : float;
  }

  let aux pred atoms =
    List.exists (fun psi -> match SL.view psi with
      | Predicate (name, _, _) -> String.equal name (InductiveDefinition.name pred)
      | _ -> false
    ) atoms

  let get_others self phi pred =
    M.remove pred self
    |> M.filter (fun pred _ -> aux pred (Derivation.negated phi pred))
    |> M.values
    |> List.concat_map Derivation.leafs

  let size res = M.fold (fun _ x acc -> acc + Derivation.cardinal x) res 0

  let is_stable res res' =
    M.for_all (fun pred d ->
      let d' = M.find pred res' in
      Derivation.is_stable d d'
    ) res

  let is_fixpoint n phi res =
    M.for_all (fun pred d ->
      let others = get_others res phi pred in
      Derivation.is_fixpoint n pred phi d others
    ) res

  let unfold n res = M.map (Derivation.unfold n) res

  let compute_size res = M.map Derivation.size res

  let initial g =
    G.fold_vertex (fun v acc ->
      M.add v (Derivation.initial v) acc
    ) g M.empty

  let debug res = M.iter Derivation.debug res

  let show res =
    M.iter (fun pred d ->
      Logger.debug "Predicate %s:\n" (InductiveDefinition.name pred);
      Derivation.show d;
      Logger.debug "\n\n";
    ) res

end

exception Termination of int * Result.t

let rec compute_fixpoint ?(n=1) res phi =
  Logger.debug "Iteration %d (cardinality %d)\n" n (Result.size res);
  let res' = Result.unfold n res in
  Result.debug res';
  (*Result.show res';*)

  let is_stable = Result.is_stable res res' in
  let is_fixpoint_prev = Result.is_fixpoint n phi res in (* TODO: n-1 *)
  let is_fixpoint_curr = Result.is_fixpoint n phi res' in

  Logger.debug "  - stable: %b\n" is_stable;
  Logger.debug "  - fixpoint (prev): %b\n" is_fixpoint_prev;
  Logger.debug "  - fixpoint (curr): %b\n" is_fixpoint_curr;

  if is_stable && is_fixpoint_prev && false then res (* TODO: sub-optimal *)
  else if is_stable && is_fixpoint_curr then res'
  else begin
    (if n > 0 && n mod 5 = 0 then
    Logger.warning "Small model search does not terminated after %d steps. \
    The system of inductive definitions is likely not flat." n);
    if n > 10 then raise @@ Termination (n, res)
    else compute_fixpoint ~n:(n+1) res' phi
  end

let debug_results res sizes =
  Logger.debug "Results:\n";
  Result.M.iter (fun id n -> Logger.debug  "- %s: %f\n" (InductiveDefinition.name id) n) sizes;
  Result.debug res

let compute g phi =
  Logger.debug "Computing small models of predicates\n";
  let res0 = Result.initial g in
  try
    let res = compute_fixpoint res0 phi in
    let sizes = Result.compute_size res in
    debug_results res sizes;
    sizes
  with Termination (n, res) ->
    let sizes = Result.compute_size res in
    debug_results res sizes;
    Exceptions.unknown_result
      ~reason:"Small model search incomplete"
      ~details:(Format.asprintf "Search does not terminated after %d steps" n)
