
open SL
open SLID

let declare_sort = Sort.smt2_decl
let declare_struct = MemoryModel.StructDef.smt2_decl
let declare_pred pred = InductiveDefinition.smt2_decl pred


(** Compute the minimal set of predicates that need to be defined:
    1. It appears directly in formula.
    2. It is used to define another predicate already in the set. *)
let used_predicates phi =
  let direct = get_inductive_definitions ~original:true phi in
  let indirect =
    List.concat_map (fun id -> GlobalSID.dependencies ~original:true (InductiveDefinition.name id)) direct
  in
  InductiveDefinition.MonoList.unique (direct @ indirect)
  |> List.sort (fun p q -> String.compare (InductiveDefinition.name p) (InductiveDefinition.name q))

(** Compute the minimal set of structures that need to be defined:
    1. It appears in a points-to in formula.
    2. It appears in a points-to in some defined predicate. *)
let used_structures phi preds =
  let formulae = phi :: List.concat_map InductiveDefinition.cases preds in
  List.concat_map (SL.select_subformulae SL.is_pointer) formulae
  |> List.map (fun pto -> match SL.view pto with SL.PointsTo (_, s, _) -> s)
  |> MemoryModel.StructDef.MonoList.unique

let used_sorts phi structs =
  let sorts = SL.get_all_sorts ~with_nil:false phi in
  sorts @ List.concat_map MemoryModel.StructDef.get_sorts structs
  |> List.filter (fun sort -> not @@ Sort.is_builtin sort)
  |> Sort.MonoList.unique

let generate_logic_string phi =
  let qf = if SL.is_quantifier_free phi then "" else "QF_" in
  let id = if SLID.has_user_defined_predicates phi then "ID" else "" in
  Format.asprintf "(set-logic %s)" (qf ^ "SH" ^ id)

let declare_heap_sort heap_sort =
  if HeapSort.is_empty heap_sort then ""
  else HeapSort.to_smt2_decl heap_sort

let generate_definitions phi heap_sort =
  let open PrintUtils in
  let predicates = used_predicates phi in
  let structs = used_structures phi predicates in
  let sorts = used_sorts phi structs in
  let heap_sort = HeapSort.restriction sorts heap_sort in
  (generate_logic_string phi)
  +++ (String.concat "\n" @@ List.map declare_sort sorts)
  (*+++ String.concat "\n" @@ List.map declare_struct structs*)
  +++ (MemoryModel.StructDef.smt2_decl_group structs)
  +++ (declare_heap_sort heap_sort)
  +++ (String.concat "\n\n" @@ List.map declare_pred predicates)
  (*++ "(set-option :use-freed-predicate)"*)

let output_benchmark ?source ?status path (input : ParserContext.t) =
  let phi = SL.mk_and input.assertions in
  let options =
    (* TODO: handle other built-in elements properly
    if has_builtin_predicates phi then Some "(set-option :use-builtin-definitions)"\n*)
    Option.some @@ generate_definitions phi input.heap_sort
  in
  let pred_sigs = GlobalSID.get_signatures () in
  output_benchmark ~pred_sigs ?source ?status ?options path phi
