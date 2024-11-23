(* Built-in list-segment predicate.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open SMT
open SL
open Translation_sig
open MemoryModel
open StackHeapModel

module G = SL_graph
module SH = StackHeapModel

module Logger = Logger.Make(struct let name = "Translation:LS" let level = 1 end)

let loc_ls = Sort.loc_ls

let next = Field.mk "next" loc_ls

let struct_ls = StructDef.mk "LS_t" "c_ls" [next]

(** Constructor *)

let check_instantiation sort def =
  match StructDef.signature def with
  | [sort'] when Sort.equal sort sort' -> Result.Ok [def]
  | [_] -> Result.Error "cannot build linked structure"
  | _ -> Result.Error "ls can be build only using structure with single field"

let get_target sort heap_sort =
  let target =
    if not @@ Sort.is_nil sort then HeapSort.find_target sort heap_sort
    else struct_ls
  in
  check_instantiation sort target

module Self0 = struct

let name = "ls"

let signature = [loc_ls; loc_ls]

let arity = 2

let default_instantiation = [struct_ls]

let struct_defs = [struct_ls]

let heap_sort = HeapSort.of_list [(loc_ls, struct_ls)]

let unique_footprint = true

let next_field def =
  let open StructDef in
  List.hd def.fields

let is_ls_sort heap_sort sort =
  Result.is_ok @@ get_target sort heap_sort

let instantiate heap_sort arg_sorts =
  get_target (List.hd arg_sorts) heap_sort

let term_bound phi heap_sort x =
  if not @@ is_ls_sort heap_sort @@ SL.Term.get_sort x then 0.0
  else match SL.classify_fragment phi with
    | Atomic | SymbolicHeap_SAT -> 1.0
    | SymbolicHeap_ENTL -> 1.0
    | Positive | Arbitrary -> 2.0

let rules ([x; y], [def]) =
  let sort = SL.Variable.get_sort x in
  let [x; y] = List.map SL.Term.of_var [x; y] in
  [
    SL.mk_eq [x; y];
    SL.mk_star [
      SL.mk_distinct [x; y];
      SL.mk_exists' [sort] (fun [n] ->
        (SL.mk_star [
          SL.mk_pto_struct x def [n];
          SL.mk_predicate "ls" [n; y] ~structs:[def]
        ]))
    ]
  ]

module Bound = struct

  type t = Interval.t

  let show = Interval.show

  let compute g phi ([x; y], [def]) loc_bounds =
    try
      let sort = SL.Term.get_sort x in
      let max = LocationBounds.total sort loc_bounds in
      let next = next_field def in
      let heap_sort = HeapSort.of_list [(sort, def)] in
      PathBound.compute g phi heap_sort next x y max
    with Not_found -> (0, 0)

end

let sl_graph ([x; y], [def]) =
  if not @@ SL.Term.equal x y
  then G.add_edge_e G.empty (x, Path (next_field def), y)
  else G.empty

let preprocess _ [x; y] =
  if SL.Term.is_nil x then Some (SL.mk_eq [x; y])
  else None

let model_check ([x; y], [def]) sh =
  let src = SH.eval sh x in
  let dst = SH.eval sh y in
  let field = next_field def in
  SH.has_path sh ~field ~src ~dst

let compute_footprints ([x; y], [def]) sh =
  let src = SH.eval sh x in
  let dst = SH.eval sh y in
  let field = next_field def in
  Footprint.of_list @@ SH.get_path sh ~field ~src ~dst

module Translation (Encoding : ENCODING) = struct

  open Encoding
  open ReachabilityEncoding.Make(Encoding)

  let translate (ctx : Encoding.Context.t) ([x'; y'], [def]) domain [x; y] bound =
    Logger.debug "Bound ls(%s) = %s\n" (SMT.show_list [x; y]) (Bound.show bound);

    let next = next_field def in
    let sort = SL.Term.get_sort x' in

    let heap = HeapEncoding.field_encoding ctx.heap next in
    let reach = reach heap x y bound in
    let domain_def = SMT.Sets.mk_eq [domain; path ctx heap x y bound] in
    let types = Locations.mk_set_of_type ctx.locs domain sort in

    let semantics = Boolean.mk_and [reach; domain_def; types] in
    let axioms = Boolean.tt in
    let footprints = [path ctx heap x y bound] in
    (semantics, axioms, footprints)

end

end

module Self = BuiltinBuilder.Make(Self0)
include Self

let register () = SID.register (module Self : ID_sig.BUILTIN)

(** API *)

let mk_pto x ~next = SL.mk_pto x next

let mk x ~sink = SL.mk_predicate "ls" [x; sink] ~structs:[struct_ls]

let mk' ?def x ~sink =
  let sort = SL.Term.get_sort x in
  let def = Option.value def ~default:struct_ls in
  match check_instantiation sort def with
    | Ok _ -> SL.mk_predicate "ls" [x; sink] ~structs:[def]
    | Error str -> failwith str
