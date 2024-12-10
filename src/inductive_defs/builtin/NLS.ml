(* Built-in nested list predicate.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open SL
open SMT
open MemoryModel
open StackHeapModel
open Translation_sig

module G = SL_graph
module SH = StackHeapModel

module Logger = Logger.Make(struct let name = "Translation:NLS" let level = 1 end)

(** Memory model *)

let loc_ls = Sort.loc_ls
let loc_nls = Sort.mk_loc "NLS_t" ~aliases:["RefNll_t"]

let top = Field.mk "top" loc_nls
let next = Field.mk "next" loc_ls

let struct_nls = StructDef.mk "NLS_t" "c_nls" [top; next]

let mk_pto_nls x top next = SL.mk_pto_struct x struct_nls [top; next]

(** Helper functions *)

(** Return sorts corresponding 'NLS_t' and 'LS_t'. *)
let sorts top_node = match StructDef.signature top_node with
   | [top; next] -> top, next

(** Return fields corresponding 'NLS_t' and 'LS_t'. *)
let fields top_node =
  let open StructDef in
  List.nth top_node.fields 0, List.nth top_node.fields 1

(** Definition *)

module Self0 = struct

  let name = "nls"

  let arity = 3

  let default_instantiation = [struct_nls; LS.struct_ls]

  (** TODO: can both sorts be same? *)
  let get_target sort heap_sort =
    let target = HeapSort.find_target sort heap_sort in
    match StructDef.signature target with
    | [sort_top; sort_next] when Sort.equal sort sort_top ->
      let ls_node = LS.get_target sort_next heap_sort in
      Result.bind ls_node (fun [node] -> Result.Ok [target; node])
    | [_; _] -> Result.Error "cannot build linked structure"
    | _ -> Result.Error "nls can be build only using structure with two field"

  let is_nls_sort heap_sort sort =
    Result.is_ok @@ get_target sort heap_sort

  let instantiate heap_sort arg_sorts =
    if List.length arg_sorts != 3 then Result.Error "incorrect arity (expected 3)"
    else get_target (List.hd arg_sorts) heap_sort

  let struct_defs = [struct_nls]

  let heap_sort = HeapSort.of_list [(loc_nls, struct_nls)]

  let unique_footprint = true

  let term_bound phi heap_sort x =
    if not @@ is_nls_sort heap_sort @@ SL.Term.get_sort x then 0.0
    else match SL.classify_fragment phi with
      | Atomic | SymbolicHeap_SAT -> 1.0
      | SymbolicHeap_ENTL -> 1.0
      | Positive | Arbitrary -> 2.0

  let rules ([x; y; z], [top_node; next_node]) =
    let sort_top, sort_next = sorts top_node in
    let [x; y; z] = List.map SL.Term.of_var [x; y; z] in
    [
      SL.mk_eq [x; y];
      SL.mk_star [
        SL.mk_distinct [x; y];
        SL.mk_exists' [sort_top; sort_next] (fun [t; n] ->
          (SL.mk_star [
            mk_pto_struct x top_node [t; n];
            SL.mk_predicate "nls" ~structs:[top_node] [t; y];
            SL.mk_predicate "ls" ~structs:[next_node] [n; z]
          ]))
      ]
    ]

  module Bound = struct

    type t = {
      top_path : Interval.t;
      nested_paths : Interval.t list; (* Bounds for first n nested paths *)
      other: Interval.t;              (* Bound for other nested path *)
    }

    let show b =
      Format.asprintf "<top: %s | nested : %s; default: %s>"
        (Interval.show b.top_path)
        (Interval.show_list ~separator:";" b.nested_paths)
        (Interval.show b.other)

    let compute g phi ([x; y; z], [top_node; next_node]) loc_bounds =
      let top_sort, next_sort = sorts top_node in
      let ls_bound =
        try (LocationBounds.total next_sort loc_bounds) + 1
        with Not_found -> 1
      in
      let max = LocationBounds.total top_sort loc_bounds in
      let partial_path = PathBound.partial_path g top x y in
      let path_sort = HeapSort.of_list [(top_sort, top_node); (next_sort, next_node)] in
      let top_bound = PathBound.compute g phi path_sort top x y max in
      let concrete, used =
        List.fold_right
          (fun loc (acc, used) ->
            try
              let bound = PathBound.compute g phi path_sort next loc z ls_bound in
              let bound = Interval.max 1 bound in
              let used = used + (fst bound) - 1 in
              bound :: acc, used
            with Not_found -> (acc, used)
          ) partial_path ([], 0)
      in
      let default = Interval.max 1 (0, ls_bound - used) in
      {top_path=top_bound; nested_paths=concrete; other=default}

  end

  let sl_graph ([x; y; z], [def; _]) =
    let top, next = fields def in
    if SL.Term.equal x y then G.empty
    else
      let g = G.add_edge_e G.empty (x, Path next, z) in
      G.add_edge_e g (x, Path top, y)

  let preprocess _ [x; y; z] =
    if SL.Term.is_nil x then Some (SL.mk_eq [x; y])
    else None

  let model_check (vars, [def; _]) sh =
    let top, next = fields def in
    let [src; dst; sink] = List.map (SH.eval sh) vars in
    if Location.equal src dst then true
    else
      let domain = SH.get_nested_path sh ~src ~dst ~sink ~field1:top ~field2:next in
      SH.has_nested_path sh ~src ~dst ~sink ~field1:top ~field2:next
      && Footprint.disjoint_list @@ List.map Footprint.of_list domain

  let compute_footprints (vars, [def; _]) sh =
    let top, next = fields def in
    let [src; dst; sink] = List.map (SH.eval sh) vars in
    if Location.equal src dst then Footprint.empty
    else
      Footprint.of_list @@ List.concat
        (SH.get_nested_path sh ~src ~dst ~sink ~field1:top ~field2:next)

module Translation (Encoding : ENCODING) = struct

  open Bound
  open Encoding
  open ReachabilityEncoding.Make(Encoding)

  let nested_path ctx selector1 selector2 source sink nested_sink bound =
    (** Nested path constructor *)
    let cons = (fun i x ->
      let b =
        try List.nth bound.nested_paths i
        with Failure _ -> bound.other
      in
      path ctx selector2 x nested_sink b)
    in
    path_cons ctx selector1 cons source sink bound.top_path

  let translate (ctx : Encoding.Context.t) (_, [top_node; next_node]) domain [x; y; z] bounds =
    let top, next = fields top_node in
    let nls_sort, ls_sort = sorts top_node in

    Logger.debug "Bound nls(%s) = %s\n" (SMT.show_list [x; y; z]) (Bound.show bounds);

    let h_next = HeapEncoding.field_encoding ctx.heap next in
    let h_top = HeapEncoding.field_encoding ctx.heap top in

    let top_reach = reach h_top x y bounds.top_path in
    let top_path = path ctx h_top x y bounds.top_path in
    let domain_def = Sets.mk_eq [domain; nested_path ctx h_top h_next x y z bounds] in
    let top_loc_bound = LocationBounds.total nls_sort ctx.location_bounds in
    let types = Locations.mk_set_of_type ctx.locs top_path nls_sort in

    let i_bounds = Interval.meet_list (bounds.other :: bounds.nested_paths) in
    let i_bounds = Interval.minus i_bounds (1, 1) in

    let invariant = Quantifier.mk_forall' [ctx.loc_sort] (fun [loc] ->
      let precond = Sets.mk_mem loc top_path in
      let postcond =
        typed_reach ctx h_next (HeapEncoding.mk_succ ctx.heap next loc) z loc_ls i_bounds
      in
      Boolean.mk_implies precond postcond)
    in

    (* Nested list are disjoint *)
    let nested_disjoint = Quantifier.mk_forall' [ctx.loc_sort; ctx.loc_sort] (fun [loc1; loc2] ->
      let allocated_loc1 = Sets.mk_mem loc1 domain in
      let allocated_loc2 = Sets.mk_mem loc2 domain in
      let down_loc1 = HeapEncoding.mk_succ ctx.heap next loc1 in
      let down_loc2 = HeapEncoding.mk_succ ctx.heap next loc2 in
      let downs_eq = SMT.mk_eq [down_loc1; down_loc2] in
      let distinct = SMT.mk_distinct [loc1; loc2] in
      let cond = Boolean.mk_and [allocated_loc1; allocated_loc2; distinct; downs_eq] in
      let downs_not_alloc = Boolean.mk_not @@ Sets.mk_mem down_loc1 domain in
      Boolean.mk_implies cond downs_not_alloc)
    in

    let empty =
      Boolean.mk_and [
        SMT.mk_eq [x; y];
        Sets.mk_eq_empty domain
      ]
    in
    let non_empty = Boolean.mk_and [
      SMT.mk_distinct [x; y];
      top_reach;
      domain_def;
      invariant; nested_disjoint;
      types
    ]
    in
    let semantics = Boolean.mk_or [empty; non_empty] in

    let axioms = Boolean.tt in

    let footprints = [nested_path ctx h_top h_next x y z bounds] in

    (semantics, axioms, footprints)

  end
end

module Self = BuiltinBuilder.Make(Self0)
include Self

let register () = SID.register (module Self : ID_sig.BUILTIN)

(** API *)

let mk_pto x ~top ~next = SL.mk_pto_struct x struct_nls [top; next]

let mk root ~sink ~bottom =
  SL.mk_predicate "nls" [root; sink; bottom] ~structs:[struct_nls; LS.struct_ls]
