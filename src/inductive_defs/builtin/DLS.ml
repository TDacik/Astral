(* Built-in doubly-linked list-segment predicate.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open SL
open SMT
open MemoryModel
open Translation_sig
open StackHeapModel

module G = SL_graph
module SH = StackHeapModel

module Logger = Logger.Make(struct let name = "Translation:DLS" let level = 1 end)

(** Memory model *)

let loc_dls = Sort.mk_loc "DLS_t" ~aliases:["RefDll_t"]

let next = Field.mk "next" loc_dls
let prev = Field.mk "prev" loc_dls

let struct_dls = StructDef.mk "DLS_t" "c_dls" [next; prev]

let mk_pto_dls x next prev = SL.mk_pto_struct x struct_dls [next; prev]

(** Definition *)

module Self0 = struct

let name = "dls"

let arity = 4

let default_instantiation = [struct_dls]

let struct_defs = [struct_dls]

let heap_sort = HeapSort.of_list [(loc_dls, struct_dls)]

let unique_footprint = true

(** Instantiation *)

(** Return first field as 'next', and second as 'prev' *)
let fields def =
  let open StructDef in
  List.nth def.fields 0, List.nth def.fields 1

let check_instantiation sort def =
  match StructDef.signature def with
  | [sort1; sort2] when Sort.equal sort sort1 && Sort.equal sort sort2 -> Result.Ok [def]
  | [_; _] -> Result.Error "cannot build linked structure"
  | _ -> Result.Error "dls can be build only using structure with two fields"

let get_target heap_sort sort =
  let target = HeapSort.find_target sort heap_sort in
  check_instantiation sort target

let is_dls_sort heap_sort sort =
  Result.is_ok @@ get_target heap_sort sort

let instantiate heap_sort arg_sorts =
  get_target heap_sort (List.hd arg_sorts)

let term_bound phi heap_sort x =
  if not @@ is_dls_sort heap_sort @@ SL.Term.get_sort x then 0.0
  else match SL.classify_fragment phi with
    | Atomic | SymbolicHeap_SAT -> 1.0
    | SymbolicHeap_ENTL -> 1.0
    | Positive | Arbitrary -> 1.5


let rules ([x; y; px; ny], [def]) =
  let sort = SL.Variable.get_sort x in
  let [x; y; px; ny] = List.map SL.Term.of_var [x; y; px; ny] in
  [
    SL.mk_star [
      SL.mk_eq [x; ny];
      SL.mk_eq [px; y];
    ];
    SL.mk_star [
      SL.mk_distinct [x; ny];
      SL.mk_distinct [px; y];
      SL.mk_exists' [sort] (fun [n] ->
        (SL.mk_star [
          mk_pto_struct x def [n; px];
          SL.mk_predicate "dls" [n; y; x; ny] ~structs:[def]
        ]))
    ]
  ]

module Bound = struct

  type t = Interval.t

  let show = Interval.show

  let compute g phi ([x; y; _; _], [def]) loc_bounds =
    let sort = SL.Term.get_sort x in
    let max = LocationBounds.total sort loc_bounds in
    let next, prev = fields def in
    let path_sort = HeapSort.of_list [(sort, def)] in
    let next_bound = PathBound.compute g phi path_sort next x y max in
    let prev_bound = PathBound.compute g phi path_sort prev y x max in
    Interval.join next_bound prev_bound

end

let sl_graph ([x; y; _; _], [def]) =
  let next, prev = fields def in
  let g = G.add_edge_e G.empty (x, Path next, y) in
  G.add_edge_e g (y, Path prev, x)

let preprocess _ [x; y; px; ny] =
  if SL.Term.is_nil x || SL.Term.is_nil y
  then Some (SL.mk_eq [x; y; px; ny])
  else None

let compute_footprints (vars, [def]) sh =
  let next, _ = fields def in
  let [x; y; px; ny] = List.map (SH.eval sh) vars in
  if SH.Location.equal x ny || SH.Location.equal y px then Footprint.empty
  else Footprint.of_list (y :: SH.get_path ~field:next ~src:x ~dst:y sh)

let model_check (vars, [def]) sh =
  let next, prev = fields def in
  let [x; y; px; ny] = List.map (SH.eval sh) vars in
  if SH.Location.equal x ny && SH.Location.equal y px then true
  else
    let footprint = compute_footprints (vars, [def]) sh in
    let path_forward = SH.get_path ~field:next ~src:x ~dst:ny sh in
    let path_backward = SH.get_path ~field:prev ~src:y ~dst:px sh in
    List.equal Location.equal path_forward (List.rev path_backward)
    && not @@ Footprint.mem px footprint
    && not @@ Footprint.mem ny footprint
    && Footprint.mem x footprint
    && Footprint.mem y footprint
    && Location.equal (SH.succ_field sh prev x) px
    && Location.equal (SH.succ_field sh next y) ny

module Translation (Encoding : ENCODING) = struct

  open ReachabilityEncoding.Make(Encoding)
  open Encoding.Context

  let translate (ctx : Encoding.Context.t) ([x'; y'; px'; ny'], [def]) domain [x; y; px; ny] bounds =
    let next, prev = fields def in
    let dls_sort = SL.Term.get_sort x' in

    Logger.debug "Bound dls(%s) = %s\n" (SMT.show_list [x; y; px; ny]) (Bound.show bounds);
    let h_next = HeapEncoding.field_encoding ctx.heap next in
    let h_prev = HeapEncoding.field_encoding ctx.heap prev in

    let empty = Boolean.mk_and [
      Boolean.mk_eq [x; ny];
      Boolean.mk_eq [y; px];
      Sets.mk_eq_empty domain;
    ]
    in
    let domain_def = Sets.mk_union ctx.fp_sort [path ctx h_next x y bounds; Sets.mk_singleton y] in
    (* TODO: path quantifier *)
    let invariant = Quantifier.mk_forall' [ctx.loc_sort] (fun [n] ->
      let precond = Boolean.mk_and [Sets.mk_mem n domain; Boolean.mk_distinct [n; y]] in
      let conseq = Boolean.mk_eq [n;
        HeapEncoding.mk_succ ctx.heap prev @@ HeapEncoding.mk_succ ctx.heap next n
      ]
      in
      Boolean.mk_implies precond conseq
    )
    in
    let nonempty = Boolean.mk_and [
      Boolean.mk_distinct [x; ny];
      Boolean.mk_distinct [y; px];
      Boolean.mk_not @@ Sets.mk_mem px domain; (* Predecessor of x is not allocated *)
      Boolean.mk_not @@ Sets.mk_mem ny domain; (* Successor of y is not allocated *)

      Boolean.mk_eq [px; HeapEncoding.mk_succ ctx.heap prev x];
      Boolean.mk_eq [ny; HeapEncoding.mk_succ ctx.heap next y];

      reach h_next x y bounds;
      Locations.mk_set_of_type ctx.locs domain dls_sort;
      Sets.mk_eq [domain; domain_def];
      invariant;
     ]
    in
    let semantics = Boolean.mk_or [empty; nonempty] in
    let axioms = Boolean.tt in
    let footprint = Boolean.mk_ite
      (Boolean.mk_and [Boolean.mk_eq [x; ny]; Boolean.mk_eq [y; px]])
      (Sets.mk_empty ctx.fp_sort)
      (domain_def)
    in
    (semantics, axioms, [footprint])
end

end

module Self = BuiltinBuilder.Make(Self0)
include Self

let register () = SID.register (module Self : ID_sig.BUILTIN)

(** API *)

let mk_pto x ~next ~prev = SL.mk_pto_struct x struct_dls [next; prev]

let mk root ~sink ~root' ~sink' =
  SL.mk_predicate "dls" [root; sink; sink'; root'] ~structs:[struct_dls]

let mk' def ~root ~sink ~root' ~sink' =
  let sort = SL.Term.get_sort root in
  match Self0.check_instantiation sort def with
    | Ok _ -> SL.mk_predicate "dls" [root; sink; sink'; root'] ~structs:[def]
    | Error str -> failwith str
