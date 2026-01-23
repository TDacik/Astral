module Logger = Logger.Make (struct let name = "CallAutomaton" let level = 1 end)

module State = struct

  type t = {
    id : Int.t;
    predicate : InductiveDefinition.t;
    global: SL.Variable.t List.t;
    params: SL.Variable.t List.t;
    existentials: SL.Variable.t List.t;
    invariant: SL.Set.t;
    accepting_condition : SL.Set.t;
  } [@@deriving compare, equal]

  let call_formula s =
    let params = List.map SL.Term.of_var s.params in
    let call = InductiveDefinition.mk_call s.predicate params in
    SL.mk_exists s.existentials call

  let invariant s =
    SL.Set.elements s.invariant
    |> SL.mk_star
    |> SL.mk_exists s.existentials

  let acc_condition s =
    SL.Set.elements s.invariant
    |> SL.mk_star
    |> SL.mk_exists s.existentials

  let show_acc_cond state =
    SL.Set.elements state.accepting_condition
    |> List.map (fun psi -> Format.asprintf "(%s)" (SL_printer.pretty_symbolic_heap psi))
    |> String.concat (" " ^ !UnicodeSymbols.or_ ^ " ")

  let show state =
    Format.asprintf "Call: %s\ninvariant: %s\naccept: %s%s"
      (SL_printer.pretty_symbolic_heap @@ call_formula state)
      (SL_printer.pretty_symbolic_heap @@ SL.mk_star @@ SL.Set.elements state.invariant)
      (show_acc_cond state)
      (if state.id > 0 then Format.asprintf "\n(unfolded #%d)" state.id else "")

  let compare state1 state2 =
    if SL.(===) (call_formula state1) (call_formula state2)
       && SL.(===) (invariant state1) (invariant state2)
       && SL.(===) (acc_condition state1) (acc_condition state2)
       && Int.equal state1.id state2.id
    then 0
    else compare state1 state2

  let equal state1 state2 = compare state1 state2 = 0

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

  (** Construction *)

  let initial predicate = {
    id = 0;
    predicate = predicate;
    global = SL.Variable.nil :: predicate.header;
    params = predicate.header;
    existentials = [];
    invariant = SL.Set.empty;
    accepting_condition = SL.Set.of_list @@ InductiveDefinition.cases ~base_only:true predicate;
  }

  let is_accepting state = not @@ SL.Set.is_empty state.accepting_condition

  let successor_rules state =
    InductiveDefinition.cases ~refresh:true ~params:(List.map SL.Term.of_var state.params) state.predicate

  let prune_invariant params invariant =
    SL.Set.filter (SL.is_ground ~ground:params) invariant

  let check_sat atoms =
    let formula = SL.mk_star atoms in
    not @@ SL_graph.has_contradiction @@ SL_graph.compute formula

  let compute_successor sid state rule =
    let qs, atoms = SL.as_quantified_symbolic_heap rule in
    let qs = SL.Variable.MonoList.unique (qs @ state.existentials) in
    let pure_atoms = List.filter SL.is_pure atoms in
    if check_sat (SL.Set.elements state.invariant @ pure_atoms) then
      let predicates = List.filter SL.is_predicate atoms in
      Option.some @@ List.map (fun pred ->
        let name, params = SL.as_predicate pred in
        let predicate = SID.find_user_defined sid name in
        let invariant =
          if InductiveDefinition.equal predicate state.predicate then
            prune_invariant
              (List.map SL.Term.as_var params @ state.global)
              @@ SL.Set.union state.invariant (SL.Set.of_list pure_atoms)
          else
            prune_invariant
              (List.map SL.Term.as_var params)
              @@ SL.Set.union state.invariant (SL.Set.of_list pure_atoms)
        in
        {
          id = 0;
          predicate = predicate;
          params = List.map SL.Term.as_var params;
          global = SL.Variable.nil :: state.global;
          existentials = SL.Variable.MonoList.inter qs (List.map SL.Term.as_var params);
          invariant = invariant;
          accepting_condition =
            SL.Set.of_list @@
              (InductiveDefinition.cases ~base_only:true ~params:params
               @@ SID.find_user_defined sid name);
        }
      ) predicates
    else None


end

module Transition = struct

  open State

  type t = {
    symbol : SL.t;
    input : State.t;
    output : State.t List.t;
  } [@@deriving compare, equal]

  let compare t1 t2 =
    if SL.(t1.symbol === t2.symbol)
       && (State.compare t1.input t2.input == 0)
       && (State.MonoList.compare t1.output t2.output == 0)
    then 0
    else compare t1 t2

  let equal t1 t2 = compare t1 t2 == 0

  let show t =
    Format.asprintf "%s -{%s}> %s" (State.show t.input) (SL_printer.pretty_symbolic_heap t.symbol) (State.show_list t.output)

  let make symbol input output =
    let symbol = SL.mk_star @@ SL.select_subformulae SL.is_atomic symbol in
    {symbol; input; output}

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

  let restrict_pred pred t =
    if not @@ InductiveDefinition.equal pred t.input.predicate then None
    else
      let output = List.filter (fun s -> InductiveDefinition.equal s.predicate pred) t.output in
      if List.is_empty output then None
      else Some {t with output = output}

  let is_self_loop t = List.exists (State.equal t.input) t.output

  let get_allocation t =
    SL.select_subformulae SL.is_pointer t.symbol
    |> List.map (fun psi -> match SL.as_pointer psi with (x, _, _) -> x)
    |> List.hd

  let is_breakpoint t = match SL.Term.view @@ get_allocation t with
    | Var v -> SL.Variable.MonoList.mem v t.input.global
    | _ -> failwith "TODO"

end

type t = {
  initial : State.t;
  states : State.Set.t;
  delta : Transition.Set.t;
}

let mem_state s aut = State.Set.mem s aut.states

let normalise aut s =
  match State.Set.find_opt s aut.states with
    | Some s -> s
    | None -> s

let out aut s =
  Transition.Set.filter (fun t -> State.equal t.input s) aut.delta
  |> Transition.Set.elements

let exists_selfloop aut s =
  out aut s
  |> List.exists Transition.is_self_loop

let print aut =
  Transition.Set.iter (fun t ->
    Logger.debug "T: %s\n" (Transition.show t);
  ) aut.delta

(** ===== Graph representation ==== *)

module Vertex = struct

  type t =
    | State of State.t
    | Transition of Transition.t
  [@@deriving compare, equal]

  let show = function
    | State s -> State.show s
    | Transition t -> Transition.show t

  let attributes = function
    | State s when State.is_accepting s -> [`Shape `Box; `Style `Bold]
    | State _ -> [`Shape `Box]
    | Transition t -> [`Shape `Diamond; `Label ""; `Width 0.1; `Height 0.1]

  let hash = Hashtbl.hash

  let is_breakpoint = function
    | State _ -> false
    | Transition t -> Transition.is_breakpoint t

end

module Edge = struct

  type t =
    | In of int * SL.t
    | Out of int

  let id = function In (x, _) -> x | Out x -> x

  let compare e1 e2 = Int.compare (id e1) (id e2)
  let equal e1 e2 = Int.equal (id e1) (id e2)

  let default = Out (-1)

  let attributes (_, label, _) = match label with
    | In (_, psi) -> [`Label (SL_printer.pretty_symbolic_heap psi)]
    | Out _ -> []

  let _id = ref 0

  let next_in psi = incr _id; In (!_id, psi)
  let next_out () = incr _id; Out !_id

end

module G = struct
  module Self = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge)
  include Self

  include Graph.Graphviz.Dot
    (struct
      include Self
      let graph_attributes g = [`Comment (Format.asprintf "%d" (nb_vertex g))]
      let default_vertex_attributes _ = []
      let vertex_name v = "\"" ^ Vertex.show v ^ "\""
      let vertex_attributes = Vertex.attributes

      let get_subgraph _ = None
      let edge_attributes e = Edge.attributes e
      let default_edge_attributes _ = []
    end)
end

let as_graph aut =
  let g = G.add_vertex G.empty (Vertex.State (normalise aut aut.initial)) in
  Transition.Set.fold (fun t acc ->
    let v = Vertex.Transition t in
    let g = G.add_vertex acc v in
    let g = G.add_edge_e g (Vertex.State (normalise aut t.input), Edge.next_in t.symbol,v) in
    BatList.fold_lefti
      (fun acc i o -> G.add_edge_e acc (v, Edge.next_out (), Vertex.State (normalise aut o))) g t.output
  ) aut.delta g

let list_max plus = function
  | [] -> 0
  | xs -> plus + BatList.max xs

let depth aut =
  let g = as_graph aut in (* TODO *)
  let rec traverse visited state =
    let succs = G.succ g state in
    match state with
    | State state ->
      if State.Set.mem state visited then None
      else Option.some @@ list_max 0 @@ List.filter_map (traverse @@ State.Set.add state visited) succs
    | Transition _ -> Option.some @@ list_max 1 @@ List.filter_map (traverse visited) succs
  in
  Option.get @@ traverse State.Set.empty (State aut.initial)

let as_simple_graph aut =
  Transition.Set.fold (fun t acc ->
    if Transition.is_self_loop t then acc
    else
      let v = Vertex.Transition t in
      let g = G.add_vertex acc v in
      let g = G.add_edge_e g (Vertex.State (normalise aut t.input), Edge.next_in t.symbol,v) in
      BatList.fold_lefti
        (fun acc i o -> G.add_edge_e acc (v, Edge.next_out (), Vertex.State (normalise aut o))) g t.output
  ) aut.delta G.empty


let unfold_loops_once aut =
  let open Transition in
  let self_loops = Transition.Set.elements @@ Transition.Set.filter Transition.is_self_loop aut.delta in
  List.fold_left (fun acc self_loop ->
    let source = self_loop.input in
    let duplicated = {source with id = 1} in
    (* Now copy all transitions from source, substituting source by duplicator in outputs. *)
    let outgoing = out aut source in
    let acc = {acc with states = State.Set.add duplicated acc.states} in
    List.fold_left (fun acc t ->
      let transient = {t with output = List.map (fun s -> if State.equal s source then duplicated else s) t.output} in
      let unfolded = {t with input = duplicated; output = List.map (fun s -> if State.equal s source then duplicated else s) t.output} in
      let delta = Transition.Set.add unfolded @@ Transition.Set.add transient @@ Transition.Set.remove t acc.delta in
      {acc with delta = delta}
    ) acc outgoing
  ) aut self_loops
(*
let unfold_transition g t source =
  let new_id = match state with Vertex.State (_, id) -> id + 1 in
  let duplicated = Vertex.State {source with id = new_id} in
  let unfolded_transition = {t with
    outputs = List.map (fun s -> if State.equal s source then duplicated else s) outputs
  }
  in
  let g0 = G.add_edge_




(* Unfold each loop in automton graph once. *)
let unfold_once aut =
  Transition.Set.fold (fun t acc ->
    let v = Vertex.Transition t in
    let g = G.add_vertex acc v in
    (* Add in-edge for a transition *)
    let g = G.add_edge_e g (Vertex.State (normalise aut t.input, 0), Edge.next_in t.symbol,v) in
    BatList.fold_lefti (fun acc i o ->
      if State.equal t.input o then
        (* Duplicate the state *)
        let dupl = Vertex.State (normalise aut o, 1) in
        let unfolded_transition = Vertex.Transition {t with } in
        let g0 = G.add_edge_e acc (Vertex.State (normalise aut t.input, 0), Edge.next_in t.symbol, ) in
        let g1 = G.add_edge_e g0 (v, Edge.next_out (), Vertex.State (normalise aut o, 0)) in
        List.fold_left (fun acc succ ->
          G.add_edge_e acc (dupl, Edge.next_out (), Vertex.State (normalise aut succ, 0))
        ) g1 t.output
      else
        (* Add an edge us usual. *)
        G.add_edge_e acc (v, Edge.next_out (), Vertex.State (normalise aut o, 0))
    ) g t.output
  ) aut.delta G.empty
*)
(** Split the automaton at breakpoints (transitions that allocates named variables
    or are uniquely determined by named variables). This is realised simply by
    removing such edges. *)
let split g =
  G.fold_vertex (fun v acc ->
    if Vertex.is_breakpoint v then G.remove_vertex acc v
    else acc
  ) g g

(** ===== Construction ===== *)

let rec construct_from_state sid aut state =
  let rules = State.successor_rules state in
  List.fold_left (fun aut rule ->
    let successor = State.compute_successor sid state rule in
    match successor with
      | Some s when List.length s >= 1 ->
        let s = List.map (normalise aut) s in
        let transition = Transition.make rule (normalise aut state) s in
        let aut' = {aut with states = State.Set.(union aut.states (of_list s));
                             delta = Transition.Set.add transition aut.delta}
        in
        print aut';
        let new_states = List.filter (fun s -> not @@ mem_state s aut) s in
        List.fold_left (construct_from_state sid) aut' new_states
      | _ -> aut
  ) aut rules

(** ===== Construction ===== *)

let self_projection aut =
  let pred = aut.initial.predicate in
  {aut with
      states = State.Set.filter (fun s -> InductiveDefinition.equal s.State.predicate pred) aut.states;
      delta = Transition.Set.filter_map (Transition.restrict_pred pred) aut.delta;
  }

let check_fragment aut =
  let aut = self_projection aut in
  let rec traverse_and_check visited last_breakpoint t =
    if Transition.Set.mem t visited then
      if Transition.is_self_loop t then ()
      else Exceptions.unsupported_fragment ~reason:"System of predicates is not flat" ~details:""
    (*else if not last_breakpoint && not @@ Transition.is_breakpoint t && exists_selfloop aut t.input
    then
      Exceptions.unsupported_fragment ~reason:"System of predicates is not 1-loop" ~details:""
    *)else
      let visited' = Transition.Set.add t visited in
      let last_breakpoint' = Transition.is_breakpoint t || not @@ exists_selfloop aut t.input in
      t.output
      |> List.concat_map (fun s -> out aut s)
      |> List.iter (traverse_and_check visited' last_breakpoint')
  in
  out aut aut.initial
  |> List.iter (traverse_and_check Transition.Set.empty false)
  (*
let postprocessing aut =
  List.fold_left*)

let all_accepting aut =
  State.Set.for_all (fun state -> not @@ SL.Set.is_empty state.accepting_condition) aut.states

let output path g =
  let channel = open_out path in
  G.output_graph channel g;
  close_out channel

let debug aut =
  let pred = aut.initial.predicate in
  Logger.dump output (InductiveDefinition.name pred ^ "-aut.dot") @@ as_graph aut;
  Logger.dump output (InductiveDefinition.name pred ^ "-self-aut.dot") @@ as_graph @@ self_projection aut;
  Logger.dump output (InductiveDefinition.name pred ^ "-aut-partition.dot") @@ split @@ as_graph aut;
  Logger.dump output (InductiveDefinition.name pred ^ "-aut-unfolded.dot") @@ as_graph @@ unfold_loops_once aut;
  (*Logger.dump SG.output (InductiveDefinition.name pred ^ "-simple-aut.dot") @@ SG.as_graph aut;*)
  ()

let construct_pred sid pred =
  let q0 = State.initial pred in
  let aut0 = {
    initial = q0;
    states = State.Set.singleton q0;
    delta = Transition.Set.empty}
  in
  let res = construct_from_state sid aut0 q0 in
  print res;
  debug res;
  check_fragment res;
  res
