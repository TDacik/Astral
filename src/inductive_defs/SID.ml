open ID
open ID_sig
open InductivePredicate
open MemoryModel

module Logger = Logger.Make (struct let name = "SID" let level = 2 end)

(** System of inductive definitions maps predicate identifiers to
    their definitions (either builtin or user defined. *)
include Map.Make(String)

let sid : ID.t t ref = ref empty

let struct_defs = ref StructDef.Set.empty

(** Create parser context with all builtin definitions. *)

module S = Set.Make(String)
module M = Map.Make(String)

let builtin_sorts () =
  StructDef.Set.elements !struct_defs
  |> BatList.concat_map (StructDef.get_sorts)
  |> List.fold_left (fun acc s ->
      let all_names = Sort.all_names s in
      List.fold_left (fun acc name -> M.add name s acc) acc all_names
    ) M.empty

let builtin_structs () =
  StructDef.Set.elements !struct_defs
  |> List.fold_left (fun acc s -> M.add (StructDef.show_cons s) s acc) M.empty

let builtin_heap_sort () =
  bindings !sid
  |> List.map (fun (_, Builtin (module B : BUILTIN)) -> B.heap_sort)
  |> HeapSort.union

let builtin_ids () = S.of_list @@ List.map fst @@ bindings !sid

let builtin_context () =
  let sorts = builtin_sorts () in
  let struct_defs = builtin_structs () in
  let heap_sort = builtin_heap_sort () in
  let ids = builtin_ids () in
  ParserContext.empty ~sorts ~struct_defs ~heap_sort ~ids ()

let show () =
  bindings !sid
  |> List.map (fun (_, pred) -> ID.show pred)
  |> String.concat ", "

let register (module B : BUILTIN) =
  Logger.debug "Registering ID %s\n" (B.name);
  sid := add B.name (Builtin (module B : BUILTIN)) !sid;
  struct_defs := StructDef.Set.union !struct_defs (StructDef.Set.of_list B.struct_defs)

let add name header body =
  let def = InductivePredicate.mk name header body in
  Logger.debug "Registering user-defined ID %s\n" name;
  if mem name !sid then Logger.debug "Skipping already registered ID %s\n" name
  else sid := add name (UserDefined def) !sid

let find name =
  try find name !sid
  with Not_found ->
    failwith @@ Format.asprintf "No definition for predicate %s (registered: %s)" name (show ())

let find_user_defined name = match find name with
  | UserDefined id -> id
  | _ -> raise Not_found

let fold_on_user_defined fn init =
  M.fold (fun name pred acc -> match pred with
    | Builtin _ -> acc
    | UserDefined id -> fn id acc
  ) !sid init

let is_declared name = mem name !sid

let is_builtin name =
  if not @@ mem name !sid then false
  else match find name with
    | Builtin _ -> true
    | UserDefined _ -> false

let is_user_defined name =
  if not @@ mem name !sid then false
  else match find name with
    | Builtin _ -> false
    | UserDefined _ -> true

let get_definition name = match find name with
  | Builtin _ -> failwith "TODO"
  | UserDefined id -> id

let has_unique_footprint name = match find name with
  | Builtin (module B : BUILTIN) -> B.unique_footprint
  | UserDefined id -> failwith "TODO: SID.unique_fp"

let fields name = match find name with
  | Builtin (module B : BUILTIN) -> List.concat_map StructDef.get_fields B.struct_defs
  | UserDefined id -> InductivePredicate.fields id

(** User-defined *)

let dependencies id = match find id.name with
  | Builtin _ -> []
  | UserDefined id ->
    InductivePredicate.dependencies id
    |> List.map find_user_defined



(** {2 Parsing} *)

let instantiate heap_sort name operands = match find name with
  | Builtin (module B : BUILTIN) -> B.instantiate heap_sort operands
  | UserDefined id -> Result.Ok (SL.mk_predicate name operands)

(** {2 Bounds} *)

let sl_graph name instance = match find name with
  | Builtin (module B : BUILTIN) -> B.sl_graph instance
  | UserDefined _ -> SL_graph0.empty

let term_bound phi heap_sort x = fold (fun name pred acc ->
   let bound = match pred with
    | Builtin (module B : BUILTIN) -> B.term_bound phi heap_sort x
    | UserDefined id -> Float.of_int @@ InductivePredicate.arity id + 1
   in
   max acc bound
) !sid Float.one

(** {2 Preprocessing *)

let preprocess name sl_graph instance = match find name with
  | Builtin (module B : BUILTIN) -> B.preprocess sl_graph instance
  | UserDefined _ -> None

(** {2 Translation} *)

module Translation (E : Translation_sig.ENCODING) = struct

  let translate name (context : E.Context.t) instance domain sxs = match find name with
    | Builtin (module B : BUILTIN) ->
      let module T = B.Translation(E) in
      let bound =
        B.Bound.compute context.sl_graph context.phi instance context.location_bounds in
      T.translate context instance domain sxs bound
    | UserDefined id ->
      Utils.internal_error
        "User-defined inductive predicate is not unfolded before translation to SMT"

end

(** {2 Model checking} *)

let model_check name instance sh = match find name with
  | Builtin (module B : BUILTIN) -> B.model_check instance sh
  | UserDefined _ -> failwith "TODO: model check UID"

let compute_footprints name instance sh = match find name with
  | Builtin (module B : BUILTIN) -> B.compute_footprints instance sh
  | UserDefined _ -> failwith "TODO: compute_footprints UID"
