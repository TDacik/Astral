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

let rec as_symbolic_heap phi = match SL.view phi with
  | _ when SL.is_atom phi -> [phi]
  | Star xs -> xs
  | Exists (_, phi) -> as_symbolic_heap phi
  | _ -> failwith @@ SL.show phi

(** Ordering on terms *)

module OrderGraph = struct
  module G = Graph.Persistent.Digraph.ConcreteBidirectional(SL.Term)
  include G
  include Graph.Oper.P(G)

  include Graph.Graphviz.Dot
    (struct
      include G

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_name v = Format.asprintf "\"%s\"" (SL.Term.show v)
      let vertex_attributes v = []

      let get_subgraph _ = None
      let edge_attributes e = []
      let default_edge_attributes _ = []
    end)

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

module Split = struct
  type t = {
    components : (SL.Variable.t * SL.t list) list;
    pure_part: SL.t;
  }

  let size split = List.length split.components

  let find_component_by_root root split =
    let matches = List.filter_map (fun (r, atoms) ->
      if SL.Variable.equal r root then Some atoms
      else None
    ) split.components
    in
    match matches with
    | [] -> split.pure_part
    | [c] -> SL.mk_star (as_symbolic_heap split.pure_part @ c)
    | _ -> assert false

end

module Vertex = struct
  type t =
    | Existential of SL.Term.t
    | Ground of SL.Variable.t [@@deriving compare, equal]

  let hash = Hashtbl.hash

  let is_ground = function Ground _ -> true | _ -> false

  let as_ground = function
    | Existential _ -> None
    | Ground t -> Some t

  let show = function
    | Existential v when SL.Term.is_sl_var v -> Format.asprintf "E %s" (SL.Term.show v)
    | Existential t -> SL.Term.show t
    | Ground t -> SL.Variable.show t

  let mk t =
    if SL.Term.is_sl_var t then Ground (SL.Term.as_var t)
    else Existential t

  let get = function
    | Existential t -> t
    | Ground t -> SL.Term.of_var t

  include Datatype.Printable(struct
    type nonrec t = t
    let show = show
  end)

end

module SplitGraph = struct

  module G = Graph.Persistent.Digraph.ConcreteBidirectional(Vertex)
  include G

  (** A vertex v is split point iff v and all of its successors are variables.
      This maintains the invariant that after spliting, the roots and leaves of
      all components all (ground) variables. *)
  let is_split_point g v =
    Vertex.is_ground v
    (*&& List.for_all Vertex.is_ground @@ G.succ g v*)

  let is_split_edge_e g e =
    is_split_point g (G.E.dst e)

  module SplitView = struct
    type t = G.t
    module V = G.V
    let iter_vertex fn g = G.iter_vertex fn g
    let iter_edges fn g =
      G.iter_edges_e (fun e ->
        if (*is_split_point g src &&*) is_split_edge_e g e
        then ()
        else fn (E.src e) (E.dst e)
      ) g
  end

  module Components = Graph.Components.Undirected(SplitView)

  let g_ref = ref G.empty

  include Graph.Graphviz.Dot
    (struct
      include G

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_name v = Format.asprintf "\"%s\"" (Vertex.show v)
      let vertex_attributes v = [
        `Style (if is_split_point !g_ref v then `Dashed else `Solid)
      ]

      let get_subgraph _ = None
      let edge_attributes e = [
        `Style (if is_split_edge_e !g_ref e then `Dashed else `Solid)
      ]
      let default_edge_attributes _ = []
    end)

  let output_graph oc g = g_ref := g; output_graph oc g

  let compute phi (existentials : SL.Variable.t list) =
    let existentials = List.map SL.Term.of_var existentials in
    let mk v =
      let open Vertex in
      if SL.Term.MonoList.mem v existentials then (Existential v)
      else if SL.Term.is_sl_var v then (Ground (SL.Term.as_var v))
      else (Existential v)
    in
    let [], atoms = SL.as_quantified_symbolic_heap phi in
    List.fold_left (fun g atom -> match SL.view atom with
      | Emp -> g
      | Eq _ | Distinct _ -> assert false
      | PointsTo (x, _, ys) ->
        List.fold_left (fun acc y -> G.add_edge acc (mk x) (mk y)) g ys
      | Predicate (_, xs, _, _) ->
        (* TODO: could be refined *)
        List_utils.diagonal_product xs
        |> List.fold_left (fun acc (u, v) -> G.add_edge acc (mk u) (mk v)) g
      | _ -> failwith @@ SL.show atom
    ) G.empty atoms

  (** Each component is a list of SL variables representing roots
      of spatial atoms. *)

  let get_component_vars g : Vertex.t list list =
    Components.components_list g

  let is_allocated g v =
    G.out_degree g v > 1

  let get_roots terms existentials g =
    let terms = List.map Vertex.mk terms in
    let module O = Graph.Oper.P(G) in
    let is_root g t =
      if not @@ G.mem_vertex g t then true
      else is_split_point g t
    in
    let mirror = O.mirror g in
    let candidates =
      List.fold_left (fun acc v ->
        if is_root mirror v then v :: acc
        else acc
      ) [] terms
    in
    let res = List.filter_map Vertex.as_ground candidates in
    SL.Variable.MonoList.diff res existentials

  let get_components cmp atoms g : (SL.Variable.t * SL.t list) list =
    let component_vars = get_component_vars g in
    match component_vars with
    | [] -> []
    | component_vars ->
      List.filter_map (fun vars ->
        let ground = List.map Vertex.get vars in
        let ground = SL.Term.MonoList.remove ground SL.Term.nil in
        match ground with
        | [] -> None
        | _ ->
          let ground_vars = List.filter_map Vertex.as_ground vars in
          let ground_vars = SL.Variable.MonoList.remove ground_vars SL.Variable.nil in
          let ground_vars = List.map SL.Term.of_var ground_vars in
          let representant = fst @@ BatList.min_max ~cmp ground_vars in
          Some (SL.Term.as_var representant,
          List.filter (fun atom -> SL.Term.MonoList.mem (SL.get_root atom) ground) atoms)
      ) component_vars

end

(** Examples preprocesing *)

(** Generate a compare function with the following property:
    If x ~~> y in some example and y ~/~> x in all examples, then x <= y. *)
let mk_order_function terms (examples : SL.t list) =
  let atoms = List.concat_map as_symbolic_heap examples in
  let og = compute_order_graph terms atoms in
  Logger.generic "order.dot" OrderGraph.output_graph og;
  is_lesser_or_eq og

(** *)
let preprocess cmp example : SL.t * SL.t =
  let atoms = as_symbolic_heap example in
  let pure, spatial = List.partition SL.is_pure atoms in
  let res0 = SL.mk_star spatial in
  SL.mk_star pure, List.fold_left (fun acc atom -> match SL.view atom with
    | Distinct _ -> acc
    | Eq xs ->
      let min_x = snd @@ BatList.min_max ~cmp xs in
      SL.map_terms (fun t ->
        if SL.Term.MonoList.mem t xs then min_x
        else t
      ) acc
  ) res0 pure

let is_admissible_frame existentials phi =
  match SL.view phi with
  | PointsTo (x, def, ys) ->
    if MemoryModel.StructDef.is_lifted def then
      true
    else (
      List.for_all SL.Term.is_sl_var (x :: ys)
      && SL.is_ground' ~forbidden:existentials phi)
  | Predicate (_, xs, _, _) ->
    List.for_all SL.Term.is_sl_var xs
    && SL.is_ground' ~forbidden:existentials phi
  | _ -> false

let compute_frame es (phis : SL.t list) = match phis with
  | [] | [_] -> (SL.emp, phis) (* No frame *)
  | _ ->
    let atoms = List.map as_symbolic_heap phis in
    let common, rest = List_utils.split_common SL.compare (is_admissible_frame es) atoms in
    (SL.mk_star common, List.map SL.mk_star rest)

type component = SL.Variable.t * SL.t list
type group = component list

type query = SL.Variable.t * SL.t list

(** Perform spliting of each example based on its split graph. Return a list
    of components for example. *)
let do_split cmp terms existentials examples : SL.Variable.t list * Split.t list =
  let components, roots_lists =
    List.split @@ List.mapi (fun i e ->
      let pure_part, spatial = preprocess cmp e in
      let g = SplitGraph.compute spatial existentials in
      Logger.generic (Format.asprintf "example_%d.dot" (i+1)) SplitGraph.output_graph g;
      let components = SplitGraph.get_components cmp (as_symbolic_heap spatial) g in
      let roots = SplitGraph.get_roots terms existentials g in
      Split.{components; pure_part}, roots
    ) examples
  in
  let roots = SL.Variable.MonoList.inter_list roots_lists in
  roots, components

(** Given a list of components for each example, group together components
    with the same root. It is important that roots may be missing in some
    examples and present in others. *)
let group_by_roots roots existentials relevant (components : Split.t list) : Query.t list =
  List.map (fun r ->
    let components = List.map (Split.find_component_by_root r) components in
    let components_vars = SL.free_vars ~with_nil:false @@ SL.mk_star components in
    let relevant_vars =
      List.filter_map (fun t ->
        if SL.Term.is_sl_var t then Some (SL.Term.as_var t) else None
      ) relevant
    in
    let vars = SL.Variable.MonoList.inter components_vars relevant_vars in
    let vars = SL.Variable.MonoList.diff vars existentials in
    let vars = r :: SL.Variable.MonoList.remove vars r in
    Query.mk vars existentials components
  ) roots
  (*let compare_root (r1, _) (r2, _) = SL.Term.compare r1 r2 in
  List.concat components
  |> BatList.group compare_root*)

(*
let generate_queries pure (groups : component list list) : query list =
  List.map (fun cs ->
    (* Root is same for all components in the group *)
    let root = fst @@ List.hd cs in
    (root, List.map (fun (_, atoms) -> SL.mk_star atoms) cs)
  ) groups
*)
(** Debug output *)

let mk_indent n =
  String.make n ' '

let mk_break n =
  String.make n '\n'

let print_atom ~indent ?(break=0) phi =
  Format.asprintf "%s-%s%s" (mk_indent indent) (SL_printer.pretty phi) (mk_break break)

let print_sh ~indent ?(break=1) phi =
  as_symbolic_heap phi
  |> List.map (fun atom -> print_atom ~indent:(indent+2) atom)
  |> String.concat "\n"

let print_sh_list ~prefix ~indent ?(break=1) phis =
  Format.asprintf "%s%s\n%s"
    (mk_indent indent)
    prefix (
      List.map (fun sh -> print_sh ~indent:(indent+2) ~break:1 sh) phis
      |> String.concat "\n"
    )
let print_components ~indent components =
  List.map (fun (root, shs) ->
    let prefix = Format.asprintf "C[%s]: " (SL.Variable.show root) in
    print_sh_list ~prefix ~indent:(indent+2) shs) components
  |> String.concat "\n"

let print_split ~indent split =
  let open Split in
  Format.asprintf "%sPure part:\n%s\n%sComponents:\n%s"
    (mk_indent indent) (print_sh ~indent:(indent+2) split.pure_part)
    (mk_indent indent) (print_components ~indent:(indent+2) split.components)

let print_splits ~indent splits =
  List.map (fun s -> print_split ~indent:(indent+2) s) splits
  |> String.concat "\n"

let show_sh phi =
  as_symbolic_heap phi
  |> List.map SL_printer.pretty
  |> String.concat "\n    * "

let show_sh_list ?(indent=4) phis =
  let indent = String.make 4 ' ' in
  let size phi = List.length @@ as_symbolic_heap phi in
  let cmp_size lhs rhs = Int.compare (size lhs) (size rhs) in
  List.sort cmp_size phis
  |> List.map show_sh
  |> String.concat (Format.asprintf "\n%s" indent)

let show_component ((root, atoms) : component) =
  Format.asprintf "      Root: %s\n      Atoms:\n         %s"
    (SL.Variable.show root)
    (show_sh @@ SL.mk_star atoms)

let show_component_list ?(indent=2) = function
  | [] -> "- []"
  | components ->
    let size c = List.length @@ snd c in
    let cmp_size lhs rhs = Int.compare (size lhs) (size rhs) in
    List.sort cmp_size components
    |> List.map show_component
    |> String.concat "\n    -- "

let show_split split =
  let open Split in
  Format.asprintf      "> Pure part:\n %s\n    > Examples:\n      %s\n"
    (SL.show split.pure_part)
    (show_component_list ~indent:6 split.components)

let pp_component_list sep (splits : Split.t list) =
  let size c = Split.size c in
  let cmp_size lhs rhs = Int.compare (size lhs) (size rhs) in
  List.sort cmp_size splits
  |> List.map show_split
  |> String.concat "\n  - "

let debug1 frame rests =
  Logger.debug "Phase 1:\n - Frame: %a\n - Rests:\n    %s\n"
    SL.pp frame
    (show_sh_list rests)

let debug2 processed =
  Logger.debug "Phase 2: preprocessing before split graph construction:\n    %s\n"
    (show_sh_list processed)

let debug3 roots (splits : Split.t list) =
  Logger.debug "Phase 3:\n";
  Logger.debug_np "  Roots: [%a]\n" SL.Variable.pp_list roots;
  Logger.debug_np "  Splits:\n %s\n" (print_splits ~indent:2 splits)

let debug4 components =
  Logger.debug "Phase 4: components:\n  %s\n"
    (pp_component_list "\n  " components)

let show_query i q =
  let open Query in
  Format.asprintf "Query %d (%a, no-learn: %b): %s" (i+1)
    SL.Variable.pp (Query.root q)
    (Query.is_simple q)
    (show_sh_list ~indent:4 q.examples)

let show_queries queries =
  List.mapi show_query queries
  |> String.concat "\n  "

let debug5 queries =
  Logger.debug "Phase 5: queries:\n  %s\n"
    (show_queries queries)

let may_distinct ctx phi x y =
  let phi = SL.mk_star [phi; SL.mk_distinct [x; y]] in
    match (Engine.solve {ctx with assertions = [phi]}).status with
    | Some (`Unsat) -> false
    | Some (`Sat) -> true
    | Some (`Unknown reason) -> failwith reason

let group_disequalities ctx phi roots =
  List_utils.diagonal_product roots
  |> List.filter_map (fun (x, y) ->
      if not @@ Sort.equal_mod_nil (SL.Term.get_sort x) (SL.Term.get_sort y) then None
      else if not @@ may_distinct ctx phi x y then None
      else Some (SL.mk_distinct [x; y])
    )

let must_allocated_vars ctx phi =
  let open ParserContext_type in
  assert (SL.is_symbolic_heap phi);
  let must_alloc phi v =
    if SL.Variable.is_nil v then false else
    let sort = SL.Variable.get_sort v in
    let struct_def = HeapSort.find_target sort ctx.heap_sort in
    let sorts = MemoryModel.StructDef.signature struct_def in
    let phi = SL.mk_exists' sorts (fun vars ->
      let atom = SL.mk_pto_struct (SL.Term.of_var v) struct_def vars in
      SL.mk_star [atom; phi]
    ) in
    match (Engine.solve {ctx with assertions = [phi]}).status with
    | Some (`Unsat) -> true
    | Some (`Sat) -> false
    | Some (`Unknown reason) -> failwith reason
  in
  let vars = SL.free_vars ~with_pure:false phi in
  List.filter (must_alloc phi) vars

let add_implicit_inequalities ctx e =
  let allocated = SL.get_terms e (*
    must_allocated_vars ctx e
    |> List.map SL.Term.of_var*)
  in
  let disequalities = group_disequalities ctx e allocated in
  let es, atoms = SL.as_quantified_symbolic_heap e in
  SL.mk_exists es @@ SL.mk_star (atoms @ disequalities)

let remove_existentials phi =
  (*let phi = match SL.view phi with
    | Exists (xs, psi) -> SL.mk_exists (List.map SL.Variable.refresh xs) psi
    | _ -> phi
  in*)
  let es, atoms = SL.as_quantified_symbolic_heap phi in
  es, SL.mk_star atoms

(** Remove variables that only appear in pure parts. *)
let unused_vars examples =
  let atoms = List.concat_map as_symbolic_heap examples in
  let all_vars = SL.get_terms @@ SL.mk_star atoms in
  let spatial = List.filter SL.is_spatial_atom atoms in
  let used_vars = SL.get_terms @@ SL.mk_star spatial in
  SL.Term.MonoList.diff all_vars used_vars, used_vars

let remove_unused_vars relevant example : SL.t =
  as_symbolic_heap example
  |> List.filter (fun a ->
        let vars = SL.get_terms a in
        not @@ SL.Term.MonoList.are_disjoint vars relevant
      )
  |> SL.mk_star

let split ctx examples =
  let examples =
    List.map (fun e ->
      let e = add_implicit_inequalities ctx e in
      let g = SL_graph.compute ~predicates:false e in
      QuantifierElimination.apply g e
    ) examples
  in
  let existentials, examples = List.split @@ List.map remove_existentials examples in
  (* Check that existentials are pairwise disjoint *)
  (*assert (
    List_utils.diagonal_product existentials
    |> List.for_all (fun (l, r) -> SL.Variable.MonoList.are_disjoint l r)
  );*)
  let existentials = List.concat existentials in
  Logger.debug "Existentials: %a" SL.Variable.pp_list existentials;
  let not_relevant, relevant = unused_vars examples in
  Logger.debug "Not relevant: %a" SL.Term.pp_list not_relevant;
  let examples : SL.t list = List.map (remove_unused_vars relevant) examples in
  let terms = SL.Term.MonoList.remove (SL.get_terms @@ SL.mk_star examples) SL.Term.nil in
  Logger.debug "Terms: %a\n" SL.Term.pp_list terms;
  let cmp = mk_order_function terms examples in
  let frame, rests = compute_frame existentials examples in
  debug1 frame rests;
  (*let pure, processed = List.split, List.map (preprocess cmp) rests in
  debug2 processed;*)
  let roots, components = do_split cmp terms existentials rests in
  debug3 roots components;
  let queries = group_by_roots roots existentials relevant components in
  (** TODO: this is just hotfix that handle queries obtained
      from non-root variables. *)
  let queries = List.filter Query.not_garbage queries in
  (*debug4 groups;
  let queries = generate_queries groups in*)
  debug5 queries;
  (frame, queries)
