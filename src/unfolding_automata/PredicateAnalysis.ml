(* Computation of bounds from automata.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module L = Logger.Make (struct let name = "Predicate analysis" let level = 1 end)

open UnfoldingAutomaton
open UnfoldingAutomaton.State
open UnfoldingAutomaton.Transition

module Logger = L

(** Dataflow analysis to compute small models from automat.
    TODO: we need to consider also loops. *)
module Fixpoint = Graph.Fixpoint.Make(G)
  (struct
    type vertex = G.V.t
    type edge = G.E.t
    type g = G.t
    type data = SL.MonoList.t
    let direction = Graph.Fixpoint.Forward
    let equal = SL.MonoList.equal
    let join = (@)
    let analyze e data =
      let source = G.E.src e in
      let dst = G.E.dst e in
      match source, G.E.label e with
        | State s, In (_, psi) -> List.map (fun psi2 -> SL.mk_star [psi; psi2]) data
        | Transition t, _ -> data
  end)

(** Root is an allocated variable with in-degree 0.
    TODO: Implement properly. *)
let compute_root pred models = List.hd pred.InductiveDefinition.header

let compute_allocated pred models =
  let non_empty = List.filter (fun g -> SL_graph.nb_allocated g > 0) models in
  List.filter (fun v ->
    List.for_all (SL_graph.must_allocated (SL.Term.of_var v)) non_empty
  ) pred.InductiveDefinition.header

let not_allocated v model =
  List.for_all (fun a ->
    let class_v = SL_graph.equivalence_class model @@ SL.Term.of_var v in
    let class_a = SL_graph.equivalence_class model a in
    let sort_v = SL.Variable.get_sort v in
    let sort_a = SL.Term.get_sort a in
    List.exists (fun x -> List.exists (fun y -> SL_graph.must_neq model x y || not @@ Sort.equal sort_a sort_v) class_a) class_v
  ) (SL_graph.must_alloc model)

let compute_dangling pred models =
  let non_empty = List.filter (fun g -> SL_graph.nb_allocated g > 0) models in
  List.filter (fun v ->
    List.for_all (not_allocated v) non_empty
  ) pred.InductiveDefinition.header

let compute_signature models =
  let open PredicateInfo in
  let non_empty = List.filter (fun g -> SL_graph.nb_allocated g > 0) models in
  List.fold_left (fun acc model ->
    let domain = List.map SL.Term.get_sort @@ SL_graph.must_alloc model in
    Signature.add (Sort.Set.of_list domain) acc
  ) Signature.empty non_empty

(** Call automaton as simple graph *)
module V = struct include State let hash = Hashtbl.hash end
module E = struct include SL let default = SL.ff end
module SG = struct
  module Self = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(V)(E)
  include Self

  include Graph.Graphviz.Dot
    (struct
      include Self
      let graph_attributes g = []
      let default_vertex_attributes _ = []
      let vertex_name v = "\"" ^ State.show v ^ "\""
      let vertex_attributes _ = [`Shape `Box]

      let get_subgraph _ = None
      let edge_attributes e = []
      let default_edge_attributes _ = []
    end)

  let as_graph aut =
    Transition.Set.fold (fun t acc ->
      BatList.fold_left (fun acc o -> add_edge_e acc (t.input, t.symbol, o)) acc t.output
    ) aut.delta empty

  let output filename (g : t) =
    let channel = open_out filename in
    output_graph channel g;
    close_out channel

end


(** Stable depth is given as the length of the longest path needed to
    reach all states with distinct invariants. *)
let stable_depth aut =
  let rec aux visited s =
    if State.Set.mem s visited then 0
    else
      let visited' = State.Set.add s visited in
      let res =
        out aut s
        |> List.concat_map (fun t -> t.output)
        |> List.map (aux visited')
        |> (fun xs -> try BatList.max xs with _ -> 0)
      in
      res + 1
  in
  aux State.Set.empty aut.initial - 1 (* TODO..... *)


(** Bound computation *)
let unfolding_depth pred aut others =
  let open InductiveDefinition in
  let depth_self = UnfoldingAutomaton.depth aut in
  Logger.debug "depth(%s): %d\n" (pred.name) depth_self;
  (* TODO: sort refinement *)
  let depth_other = BatList.max @@ List.map UnfoldingAutomaton.depth others in
  if List.for_all (fun aut -> UnfoldingAutomaton.all_accepting aut) others
  then depth_other + 1 (* implicit sink *)
  else
  depth_self + depth_other
  + 1 (* 1 for implicit sink in other *)

let check_predicate pred aut =
  let open InductiveDefinition in
  if GlobalSID.has_unique_footprint pred.name then ()
  else if UnfoldingAutomaton.all_accepting aut then ()
  else
    Exceptions.unsupported_fragment
      ~reason:"non-deterministic SID"
      ~details:(Format.asprintf "non-deterministic predicate:\n%s"
        (InductiveDefinition.show pred)
      )

let compute_pred sid pred automata =
  let aut = UnfoldingAutomaton.construct_pred sid pred in
  check_predicate pred aut;
  let init = function UnfoldingAutomaton.Vertex.State s when State.equal s aut.initial -> [SL.emp] | _ -> [] in
  let g = as_simple_graph aut in (* TODO: unfold once *)
  let res = Fixpoint.analyze init g in
  let cnt = ref 1 in
  let sm = G.fold_vertex (fun s acc -> match s with
    | State s ->
      let data = res (State s) in
      let small_models =
        List.map (fun psi -> SL.mk_star (psi :: SL.Set.elements s.accepting_condition)) data
        |> List.map (SL_graph.compute ~stars:false)
      in
      let name = Format.asprintf "%s_%d.dot" (InductiveDefinition.name pred) !cnt in
      let _ = cnt := !cnt + 1 in
      List.iter (Logger.dump SL_graph.G.output_file name) small_models;
      acc @ small_models
    | _ -> acc
  ) g [] in
  PredicateInfo.Entry.{
    root = compute_root pred sm;
    allocated = compute_allocated pred sm;
    never_allocated = compute_dangling pred sm;

    signature = compute_signature sm;

    stable_depth = stable_depth aut;
    unfolding_depth = unfolding_depth pred aut automata;
  }

let debug pred info =
  Logger.debug "Predicate %s:%s"
    (SL.show @@ InductiveDefinition.mk_call pred @@ List.map SL.Term.of_var pred.header)
    (PredicateInfo.Entry.show info)

(*
let check_fragment infos =
  let open InductiveDefinition in
  let open PredicateInfo.Entry in
  PredicateInfo.bindings infos
  |> List.for_all (fun (pred, abs) -> )
  |> List_utils.diagonal_product
  |> List.iter (fun ((pred1, abs1), (pred2, abs2)) ->
       if PredicateInfo.Signature.disjoint abs1.signature abs2.signature then ()
       else if SID_checks.are_field_distinguishable pred1 pred2 then ()
       else Exceptions.unsupported_fragment
              ~reason:"System of predicates is not flat"
              ~details:(Format.asprintf "%s and %s shares signature" pred1.name pred2.name)
     )
  *)

let compute_automata sid =
  SID.fold_user_defined (fun pred acc ->
    let aut = UnfoldingAutomaton.construct_pred sid pred in
    aut :: acc
  ) sid []

let compute sid =
  let automata = compute_automata sid in
  let res =
    SID.fold_user_defined (fun pred acc ->
      if Inlining.can_be_inlined pred.name then acc
      else
        let res = compute_pred sid pred automata in
        debug pred res;
        PredicateInfo.add pred res acc
    ) sid PredicateInfo.empty
  in
  (*check_fragment res;*)
  res
