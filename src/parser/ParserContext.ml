(* Parser context.
 *
 * author: tomas dacik (idacik@fit.vut.cz), 2022 *)

open MemoryModel
open ParserException
open ParserContext_type

module S = Set.Make(String)
module M = Map.Make(String)

type t = ParserContext_type.t

let empty ?(sorts=M.empty) ?(struct_defs=M.empty) ?(heap_sort=HeapSort.empty) ?(ids=S.empty) () =
  {
    sorts = sorts;
    struct_defs = struct_defs;
    vars = M.empty;
    heap_sort = heap_sort;

    declared_preds = ids;

    expected_status = `Unknown;
    attributes = M.empty;

    produce_models = false;
    assertions = [];
  }


let add_defs ctx1 ctx2 =
  let open ParserException in
  let disjoint_map_union eq what =
    M.union (fun key x y ->
      if eq x y then Some x
      else raise_redefined None ctx1 (Builtin what) key
    )
  in
  {
    sorts = disjoint_map_union Sort.equal Sort ctx1.sorts ctx2.sorts;
    struct_defs = disjoint_map_union StructDef.equal Structure ctx1.struct_defs ctx2.struct_defs;
    vars = disjoint_map_union Sort.equal Variable ctx1.vars ctx2.vars;
    heap_sort = HeapSort.union [ctx1.heap_sort; ctx2.heap_sort];
    declared_preds = S.union ctx1.declared_preds ctx2.declared_preds;
    expected_status = ctx1.expected_status;
    attributes = ctx1.attributes;
    produce_models = ctx1.produce_models;
    assertions = ctx1.assertions @ ctx2.assertions;
  }


(** Declarations *)

let declare_sort ?loc ctx sort =
  let name = Sort.name sort in
  if M.mem name ctx.sorts then ParserException.raise_redefined loc ctx Sort name
  else {ctx with sorts = M.add name sort ctx.sorts}

let declare_var ?loc ctx var sort =
  if var = "nil" then ParserException.raise_syntax_error loc "The name 'nil' is reserved for separation logic constant"
  else if M.mem var ctx.vars then ParserException.raise_redefined loc ctx Variable var
  else {ctx with vars = M.add var sort ctx.vars}

let find_var ?loc ctx var =
  try (SL.Variable.mk var @@ M.find var ctx.vars)
  with Not_found -> ParserException.raise_not_declared loc ctx Variable var

let type_of_var ?loc ctx var =
  try M.find var ctx.vars
  with Not_found -> ParserException.raise_not_declared loc ctx Variable var

let find_sort ?loc ctx name =
  try M.find name ctx.sorts
  with Not_found -> ParserException.raise_not_declared loc ctx Sort name

let is_declared_struct ctx name = M.mem name ctx.struct_defs

let declare_struct ?loc ctx name cons fields =
  if is_declared_struct ctx cons then ParserException.raise_redefined loc ctx Constructor cons
  else
    let def = StructDef.mk name ~cons fields in
    {ctx with struct_defs = M.add cons def ctx.struct_defs}

let find_struct_def_by_cons ?loc ctx cs_name =
  try M.find cs_name ctx.struct_defs
  with Not_found -> ParserException.raise_not_declared loc ctx Constructor cs_name

let find_struct_def_by_name ?loc ctx name =
  try
    M.bindings ctx.struct_defs
    |> List.find (fun (_, s) -> String.equal (StructDef.get_name s) name)
    |> snd
  with Not_found -> ParserException.raise_not_declared loc ctx Structure name

let declare_heap_sort ctx mapping =
  {ctx with heap_sort = HeapSort.of_list mapping}

let declare_pred ctx name =
  {ctx with declared_preds = S.add name ctx.declared_preds}

let is_declared_pred ctx name = S.mem name ctx.declared_preds

let set_expected_status ctx = function
  | "sat" -> {ctx with expected_status = `Sat}
  | "unsat" -> {ctx with expected_status = `Unsat}
  | "unknown" -> {ctx with expected_status = `Unknown}
  | other -> ParserException.raise_syntax_error None ("Unknown status '" ^ other ^ "'")

let set_attribute ctx name value = {ctx with attributes = M.add name value ctx.attributes}

let set_produce_models ctx flag = {ctx with produce_models = flag}

let add_assertion ctx phi = {ctx with assertions = phi :: ctx.assertions}

let add_vars ctx vars =
  List.fold_left
    (fun ctx var ->
      let name, sort = SL.Variable.describe var in
      declare_var ctx name sort
    ) ctx vars

(*** Accessors ***)

let get_vars ctx =
  M.bindings ctx.vars
  |> List.map (fun (name, sort) -> SL.Variable.mk name sort)

let get_sl_vars ctx = List.filter SL.Variable.is_loc (get_vars ctx)

let get_phi ctx = SL.mk_and ctx.assertions

(*** ==== Pretty-printing ==== *)

let show_sorts ctx =
  M.bindings ctx.sorts
  |> List.map (fun (name, sort) -> Format.asprintf "%s -> %s" name (Sort.show sort))
  |> String.concat ", "
  |> Format.asprintf "Sorts: {%s}"

let show_structs ctx =
  M.bindings ctx.struct_defs
  |> List.map snd
  |> List.map StructDef.show
  |> String.concat ", "
  |> Format.asprintf "Structs: {%s}"

let show_vars ctx =
  M.bindings ctx.vars
  |> List.map (fun (var, sort) -> Format.asprintf "%s : %s" var (Sort.show sort))
  |> String.concat ", "
  |> Format.asprintf "Variables: {%s}"

let show_attributes ctx =
  M.bindings ctx.attributes
  |> List.map (fun (name, value) -> Format.asprintf "%s = %s" name value)
  |> String.concat ", "
  |> Format.asprintf "Attributes: {%s}"

let show_assertions ctx =
  ctx.assertions
  |> List.map SL.show
  |> String.concat "\t\n"
  |> Format.asprintf "Assertions: {\t\n%s\n}"

let show ctx =
  Format.asprintf "%s\n  %s\n Heap sort: %s\n  %s\n  %s\n  %s\n"
    (show_sorts ctx)
    (show_structs ctx)
    (HeapSort.show ctx.heap_sort)
    (show_vars ctx)
    (show_attributes ctx)
    (show_assertions ctx)
