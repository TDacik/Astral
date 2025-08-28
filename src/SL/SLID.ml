(* Separation logic with inductive definitions.
 *
 * Those functions depends on built-in inductive predicates and cannot
 * be implemented directly in SL module.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open SL

let rec has_unique_footprint phi = match view phi with
  | Emp | Eq _ | Distinct _ | PointsTo _ | False -> true
  | Predicate (pred, _, _) -> SID.has_unique_footprint pred
  | Star xs | And xs -> List.for_all has_unique_footprint xs
  | Or _ | Exists _ | Not _ -> false
  | GuardedNeg (lhs, _) -> has_unique_footprint lhs
  | Ite (_, then_, else_) -> List.for_all has_unique_footprint [then_; else_]
  | _ -> failwith @@ ("Unique footprint of: " ^ show phi)

let has_unique_shape _ = failwith "has_unique_shape"

let rec get_structs ?(visited=[]) (phi : SL.t) =
  let atoms = SL.select_subformulae SL.is_spatial_atom phi in
  atoms |> List.concat_map (fun psi -> match SL.view psi with
       | PointsTo (_, s, _) -> [s]
       | Predicate (pred, _, _) -> SID.get_structs visited (fun visited -> get_structs ~visited) pred
       | _ -> []
     )
  |> BatList.unique_cmp ~cmp:MemoryModel.StructDef.compare

let get_structs phi = get_structs phi

let get_inductive_definitions phi =
  SL.select_subformulae (fun phi -> match SL.view phi with
      | Predicate (name, _, _) -> SID.is_user_defined name
      | _ -> false
    ) phi
  |> List.map (fun phi -> match SL.view phi with Predicate (name, _, _) -> SID.find_user_defined name)

let has_builtin_predicates phi =
  let uids =
    SL.select_subformulae (fun phi -> match SL.view phi with
      | Predicate (name, _, _) -> SID.is_builtin name
      | _ -> false
    ) phi
  in
  not @@ List.is_empty uids

let has_user_defined_predicates phi =
  not @@ List.is_empty @@ get_inductive_definitions phi

let declare_sort = Sort.smt2_decl
let declare_struct = MemoryModel.StructDef.smt2_decl
let declare_pred pred = "(define-fun-rec " ^ (InductiveDefinition.smt2_decl pred) ^ "\n)"

let generate_definitions phi =
  let sorts = SL.get_all_sorts phi in
  let structs = get_structs phi in
  let predicates = get_inductive_definitions phi in
  let (++) x y = x ^ "\n" ^ y in
  (String.concat "\n" @@ List.map declare_sort sorts)
  ++ (String.concat "\n" @@ List.map declare_struct structs)
  ++ (String.concat "\n" @@ List.map declare_pred predicates)

let output_benchmark ?source ?status path phi =
  let options =
    (* TODO: handle other built-in elements properly *)
    if has_builtin_predicates phi then Some "(set-option :use-builtin-definitions)\n"
    else if has_user_defined_predicates phi then Some ";; TODO generate definitions"
    else None
  in
  output_benchmark ?source ?status ?options path phi
