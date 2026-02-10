(* Separation logic with inductive definitions.
 *
 * Those functions depends on inductive definitions and cannot
 * be implemented directly in SL module.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open SL

let rec has_unique_footprint phi = match view phi with
  | Emp | Eq _ | Distinct _ | PointsTo _ | False -> true
  | Predicate (pred, _, _) -> GlobalSID.has_unique_footprint pred
  | Star xs | And xs -> List.for_all has_unique_footprint xs
  | Or _ | Exists _ | Not _ -> false
  | GuardedNeg (lhs, _) -> has_unique_footprint lhs
  | Ite (_, then_, else_) -> List.for_all has_unique_footprint [then_; else_]
  | _ -> failwith @@ ("Unique footprint of: " ^ show phi)

let has_unique_shape _ = failwith "has_unique_shape"

let must_allocated_terms phi =
  let get_allocated_atom atom = match SL.view atom with
    | PointsTo (x, _, _) -> [x]
    | Predicate (name, params, _) -> GlobalSID.get_must_allocated name ~params
  in
  SL.select_subformulae SL.is_spatial_atom phi
  |> List.concat_map get_allocated_atom
  |> SL.Term.MonoList.unique

(** Compute semantically dangling variables in formula:
    1. get all localy dangling variables in predicates
    2. remove those that all allocated somewhere         *)
let may_dangling_terms phi =
  let get_dangling_atom atom = match SL.view atom with
    | PointsTo (_, _, ys) -> ys
    | Predicate (name, params, _) -> GlobalSID.get_may_dangling name ~params
  in
  let allocated = must_allocated_terms phi in
  SL.select_subformulae SL.is_spatial_atom phi
  |> List.concat_map get_dangling_atom
  |> List.filter (fun v -> not @@ SL.Term.MonoList.mem v allocated)
  |> SL.Term.MonoList.unique
  |> (fun xs -> SL.Term.MonoList.remove xs SL.Term.nil)


let rec get_structs ?(visited=[]) (phi : SL.t) =
  let atoms = SL.select_subformulae SL.is_spatial_atom phi in
  atoms |> List.concat_map (fun psi -> match SL.view psi with
       | PointsTo (_, s, _) -> [s]
       | Predicate (pred, _, _) -> GlobalSID.get_structs visited (fun visited -> get_structs ~visited) pred
       | _ -> []
     )
  |> BatList.unique_cmp ~cmp:MemoryModel.StructDef.compare

let get_structs phi = get_structs phi

let has_builtin_predicates phi =
  let uids =
    SL.select_subformulae (fun phi -> match SL.view phi with
      | Predicate (name, _, _) -> GlobalSID.is_builtin name
      | _ -> false
    ) phi
  in
  not @@ List.is_empty uids

let get_inductive_definitions ?(original=false) phi =
  SL.select_subformulae (fun phi -> match SL.view phi with
      | Predicate (name, _, _) -> GlobalSID.is_user_defined name
      | _ -> false
    ) phi
  |> List.map (fun phi -> match SL.view phi with
       Predicate (name, _, _) -> GlobalSID.find_user_defined ~original name
     )

let has_user_defined_predicates phi =
  not @@ List.is_empty @@ get_inductive_definitions phi

