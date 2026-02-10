
open SL
open SLID

let declare_sort = Sort.smt2_decl
let declare_struct = MemoryModel.StructDef.smt2_decl
let declare_pred pred = "(define-fun-rec " ^ (InductiveDefinition.smt2_decl pred) ^ "\n)"


(** Compute the minimal set of predicates that need to be defined:
    1. It appears directly in formula.
    2. It is used to define another predicate already in the set. *)
let used_predicates phi =
  let direct = get_inductive_definitions ~original:true phi in
  let indirect =
    List.concat_map (fun id -> GlobalSID.dependencies ~original:true (InductiveDefinition.name id)) direct
  in
  InductiveDefinition.MonoList.unique (direct @ indirect)

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



(* TODO: how to remove unused? *)
let generate_definitions phi heap_sort =
  let predicates = used_predicates phi in
  let structs = used_structures phi predicates in
  let sorts = used_sorts phi structs in
  let heap_sort = HeapSort.restriction sorts heap_sort in
  let (++) x y = x ^ "\n\n" ^ y in
    (String.concat "\n" @@ List.map declare_sort sorts)
  ++ (String.concat "\n" @@ List.map declare_struct structs)
  ++ (HeapSort.to_smt2_decl heap_sort)
  ++ (String.concat "\n" @@ List.map declare_pred predicates)
  (*++ "(set-option :use-freed-predicate)"*)

let output_benchmark ?source ?status path (input : ParserContext.t) =
  let phi = SL.mk_and input.assertions in
  let options =
    (* TODO: handle other built-in elements properly
    if has_builtin_predicates phi then Some "(set-option :use-builtin-definitions)"\n*)
    Option.some @@ generate_definitions phi input.heap_sort
  in
  output_benchmark ?source ?status ?options path phi
