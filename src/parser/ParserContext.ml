(* Parser context.
 *
 * author: tomas dacik (idacik@fit.vut.cz), 2022 *)

open MemoryModel
open ParserUtils
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

    expected_status = `Unknown "not provided";
    attributes = M.empty;

    produce_models = false;
    assertions = [];
  }

(** Declarations *)

let declare_sort ctx sort =
  let name = Sort.name sort in
  if M.mem name ctx.sorts then raise @@ SortRedefined sort
  else {ctx with sorts = M.add name sort ctx.sorts}

let declare_var ctx var sort =
  if var = "nil" then raise @@ SyntaxError "The name 'nil' is reserved for separation logic constant"
  else if M.mem var ctx.vars then raise @@ VariableRedefined var
  else {ctx with vars = M.add var sort ctx.vars}

let find_var ctx var =
  try (SL.Variable.mk var @@ M.find var ctx.vars)
  with Not_found -> raise @@ VariableNotDeclared var

let type_of_var ctx var =
  try M.find var ctx.vars
  with Not_found -> raise @@ VariableNotDeclared var

let find_sort ctx name =
  try M.find name ctx.sorts
  with Not_found -> raise @@ SortNotDeclared name

let declare_struct ctx name cs_name fields =
  let def = StructDef.mk name cs_name fields in
  {ctx with struct_defs = M.add cs_name def ctx.struct_defs}

let is_declared_struct ctx name = M.mem name ctx.struct_defs

let find_struct_def_by_cons ctx cs_name =
  try M.find cs_name ctx.struct_defs
  with Not_found -> raise @@ ConstructorNotDeclared cs_name

let find_struct_def_by_name ctx name =
  try
    M.bindings ctx.struct_defs
    |> List.find (fun (_, s) -> String.equal (StructDef.get_name s) name)
    |> snd
  with Not_found -> raise @@ StructNotDeclared name

let declare_heap_sort ctx mapping =
  {ctx with heap_sort = HeapSort.of_list mapping}

let declare_pred ctx name =
  {ctx with declared_preds = S.add name ctx.declared_preds}

let is_declared_pred ctx name = S.mem name ctx.declared_preds

let set_expected_status ctx = function
  | "sat" -> {ctx with expected_status = `Sat}
  | "unsat" -> {ctx with expected_status = `Unsat}
  | "unknown" -> {ctx with expected_status = `Unknown "not provided"}
  | other -> raise @@ ParserError ("Unknown status '" ^ other ^ "'")

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
