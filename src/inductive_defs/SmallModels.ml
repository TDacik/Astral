(* Implementation of search of small models of inductive defintions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module Logger = Logger.MakeWithDir
  (struct
    let dirname = "small-models"
    let name = "small-models"
    let level = 1
  end)

module Abstraction = PredicateAbstraction

module SL_graph = struct
  include SL_graph

  let is_existential term : bool =
    let name = SL.Term.show term in
    String.contains name '!' (* TODO: do this properly *)

  (** In general, SL-graps are used to capture must-properties of BSL. This function
      is intended for their specific usage in context of symbolic heaps and thus defined
      here and not in SL_Graph. *)
  let is_never_allocated v g =
    if SL_graph.must_allocated v g then false
    else
      let alloc = SL_graph.must_alloc g in
      List.for_all (fun a ->
        let cls = SL_graph.equivalence_class g a in
        List.exists (SL_graph.must_neq g v) cls
      ) alloc


  let is_global term = not @@ is_existential term

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

  let nb_alloc_params g =
    SL_graph.must_alloc g
    |> List.filter (fun v -> not @@ is_existential v)
    |> List.length

  let size g =
    if SL_graph.nb_allocated g == 0 then 0.0
    else
      let alloc = Float.of_int @@ SL_graph.nb_allocated g in
      let alloc_params = Float.of_int @@ nb_alloc_params g in
      Float.div alloc alloc_params

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
  let unfold ~base_only (pred, xs) = InductiveDefinition.cases pred ~refresh:true ~base_only ~params:xs

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
    Format.asprintf "Graph: %s\nHyperdges:\n  %s\n"
      (SL_graph.show self.graph)
      (String.concat "\n  " @@ List.map Hyperedge.show @@ Hyperedge.Set.elements self.hyper_edges)

  (** Create a singleton hyper-edge representing inductive predicate call. *)
  let of_inductive_def id params =
    {
      graph = SL_graph.empty;
      hyper_edges = Hyperedge.Set.singleton @@ Hyperedge.of_formula @@ InductiveDefinition.to_formula id ~params;
    }

  let of_formula phi =
    let _, atoms = SL.as_quantified_symbolic_heap phi in
    let predicates, pointers = List.partition SL.is_predicate atoms in
    {
      graph = SL_graph.compute ~stars:false @@ SL.mk_star pointers;
      hyper_edges = Hyperedge.Set.of_list @@ List.map Hyperedge.of_formula predicates;
    }

  let disjoint_union hg1 hg2 = {
    graph = SL_graph.disjoint_union ~stars:false [hg1.graph; hg2.graph];
    hyper_edges = Hyperedge.Set.union hg1.hyper_edges hg2.hyper_edges;
  }

  let size self = SL_graph.nb_allocated ~distinct:true self.graph

  let is_graph self = Hyperedge.Set.is_empty self.hyper_edges

  let to_graph self =
    assert (is_graph self);
    self.graph

  (** Unfolding *)
  let unfold ~base_only self =
    (*Logger.debug "Unfolding\n %s\n" (show self);*)
    Hyperedge.Set.fold (fun selected acc ->
      (*Logger.debug "Unfolding edge %s\n" (Hyperedge.show selected);*)
      let untouched = {self with hyper_edges = Hyperedge.Set.remove selected self.hyper_edges} in
      let unfoldings =
        Hyperedge.unfold ~base_only selected
        |> List.map of_formula
        |> List.map (disjoint_union untouched)
      in
      unfoldings @ acc
    ) self.hyper_edges []





end

module Derivation = struct
  open Hypergraph

  module HS = Set.Make(Hypergraph)
  module GS = Set.Make(SL_graph)

  type t = {
    id : InductiveDefinition.t;
    stable : int option;

    graphs : GS.t;    (** Derived graphs *)
    worklist : HS.t;  (** Worklist of hypergraphs to be unfolded *)
    finished : HS.t;  (** Set of already (fully) unfolded hypergraphs *)
  }

  let empty id = {
    id = id;
    stable = None;

    graphs = GS.empty;
    worklist = HS.empty;
    finished = HS.empty;
  }

  let add_to_worklist self hg =
    if Hyperedge.Set.is_empty hg.hyper_edges then
      let g = SL_graph.contract @@ Hypergraph.to_graph hg in
      {self with graphs = GS.add g self.graphs}
    else if not @@ HS.mem hg self.finished then
      {self with worklist = HS.add hg self.worklist}
    else self

  let leafs self = GS.elements self.graphs

  let worklist self = HS.elements self.worklist

  (** Single step of unfolding: *)
  let unfold_step ?(base_only=false) n self =
    (* Select only those that not yet reached the bound *)
    let worklist =
      worklist self
      |> List.filter (fun h -> Hypergraph.size h <= n)
    in
    let self' = {
      self with worklist = HS.empty; (* TODO: check rest *)
                finished = HS.union self.finished self.worklist}
    in
    List.concat_map (Hypergraph.unfold ~base_only) worklist
    |> List.fold_left add_to_worklist self'

  let rec close self =
    if HS.is_empty self.worklist then self
    else close @@ unfold_step ~base_only:true 1000 self (* TODO *)

  let rec unfold n self =
    if HS.for_all (fun h -> Hypergraph.size h > n) self.worklist then
      (* TODO: close *) self
    else
      unfold n (unfold_step n self)

  let initial id =
    let params = List.map SL.Term.of_var @@ InductiveDefinition.header id in
    add_to_worklist (empty id) (Hypergraph.of_inductive_def id params)
    |> unfold 0

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

  let is_fixpoint n distinguishers phi der =
    let negated = negated phi der.id in
    let ptrs = List.filter SL.is_pointer negated in
    let cond = match ptrs with
      | [] -> true
      | _ -> List.exists SL_graph.has_existential (leafs der)
    in
    (* TODO: optinisation with nil *)
    if SID_checks.is_distinguishable_by_sort der.id distinguishers then
      List.exists (fun g -> SL_graph.nb_allocated g > 0) (leafs der) && cond
    else
      List.exists (fun g -> SL_graph.nb_allocated ~distinct:true g > 1) (leafs der) && cond

  let size der =
    leafs der
    |> List.map SL_graph.size
    |> (fun x ->
      try BatList.max ~cmp:Float.compare x
      with _ -> assert false
    )

  (** TODO: Is root a variable allocated in all models? *)
  let find_root id models =
    List.find (fun v ->
      List.for_all (SL_graph.must_allocated (SL.Term.of_var v)) models
    ) id.InductiveDefinition.header

  let find_allocated id models =
    List.filter (fun v ->
      List.for_all (SL_graph.must_allocated (SL.Term.of_var v)) models
    ) id.InductiveDefinition.header

  let find_never_allocated id models =
    List.filter (fun v ->
      List.for_all (SL_graph.is_never_allocated (SL.Term.of_var v)) models
    ) id.InductiveDefinition.header

  let compute_skeleton_fields models =
    let all_fields =
      List.concat_map SL_graph.get_fields models
      |> BatList.unique_cmp ~cmp:MemoryModel.Field.compare
    in
    let sublists = List_utils.sublists all_fields in
    (* TODO: order by size *)
    List.find (fun fields ->
      List.for_all (fun g -> SL_graph.are_skeleton_fields g fields) models
    ) sublists

  let abstraction id der =
    let leafs = leafs der in
    let non_empty = BatList.filter (fun g -> SL_graph.nb_allocated g > 0) leafs in
    let root = find_root der.id non_empty in
    let allocated = find_allocated der.id non_empty in
    let never_allocated = find_never_allocated der.id non_empty in
    let fixpoint_size = match leafs with
      | [] -> assert false;
      | ls -> BatList.max ~cmp:Float.compare @@ List.map SL_graph.size ls
    in
    let unfolding_depth =
      Int.of_float (Float.mul fixpoint_size @@ Float.of_int @@ List.length allocated) in

    let stable_size = match der.stable with
      | None -> assert false
      | Some x -> x
    in

    let skeleton_fields = compute_skeleton_fields non_empty in
    Abstraction.{id; root; allocated; never_allocated; skeleton_fields; stable_size; fixpoint_size; unfolding_depth}

  let cardinal self = HS.cardinal self.worklist

  let show ?(indent=0) self =
    (*
   GS.iter (fun g -> Logger.debug "%s" (SL_graph.show g)) self.graphs
    *)Format.printf "G: %d, W: %d, F: %d\n"
      (GS.cardinal self.graphs)
      (HS.cardinal self.worklist)
      (HS.cardinal self.finished);

    Format.printf "\nGraphs:";
    GS.iter (fun g -> Format.printf "\n  %s\n" (SL_graph.show g)) self.graphs;
    Format.printf "\nWorklist:";
    HS.iter (fun h -> Format.printf "\n  %s\n" (Hypergraph.show h)) self.worklist;
    Format.printf "\nVisited:";
    HS.iter (fun h -> Format.printf "\n  %s\n" (Hypergraph.show h)) self.finished

  let debug pred der =
  (*
    Logger.debug "Derivation %s:\n" (InductiveDefinition.name pred);
    show der;*)
    leafs der
    |> List.iteri (fun i g ->
      let name = Format.asprintf "%s_%d" (InductiveDefinition.name pred) i in
      Logger.dump SL_graph.G.output_file (name ^ ".xdot") g;
      Logger.dump SL_graph.G.output_file (name ^ "_pure.xdot") (SL_graph.remove_existentials @@ SL_graph.pure_projection g)
    )

end

let rec compute_fixpoint ?(n=1) phi distinguishers pred res =
  (if n > 100 then
    let _ = Derivation.debug pred res in
    Exceptions.unsupported_fragment
      ~reason:"incomplete small model search"
      ~details:"The fixpoint computation does not terminated after 100 steps"
  );
  let open InductiveDefinition in
  Logger.debug "%s: iteration %d\n" pred.name n;
  let res' = Derivation.unfold n res in

  let is_stable = Derivation.is_stable res res' in
  let is_fixpoint = Derivation.is_fixpoint n distinguishers phi res' in

  Logger.debug "  - stable: %b\n" is_stable;
  Logger.debug "  - fixpoint (prev): %b\n" is_fixpoint;

  let res'' = if is_stable && Option.is_none res'.stable then {res' with stable = Some (n - 1)} else res' in

  if is_stable && is_fixpoint then match res''.stable with
    | _ when Derivation.is_fixpoint (n-1) distinguishers phi res ->
      (res, Derivation.abstraction pred {res with stable = Some (n -1)})
    | _ -> (res'', Derivation.abstraction pred res'')
  else compute_fixpoint ~n:(n+1) phi distinguishers pred res''


let debug_results res =
  Logger.debug "Results:\n";
  PredicateAbstraction.M.iter (fun id a ->
      Logger.debug  "- %s: %s\n" (InductiveDefinition.name id) (Abstraction.show a)
  ) res

let compute phi distinguishers =
  Logger.debug "Computing small models of predicates\n";
  let preds = SID.get_user_defined () in
  let res =
    List.fold_left (fun acc pred ->
      let res0 = Derivation.initial pred in
      let der, res = compute_fixpoint phi distinguishers pred res0 in
      Derivation.debug pred der;
      Abstraction.M.add pred res acc
    ) Abstraction.M.empty preds
  in
  debug_results res;
  res
