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

let has_user_defined_predicates phi =
  let uids =
    SL.select_subformulae (fun phi -> match SL.view phi with
      | Predicate (name, _, _) -> SID.is_user_defined name
      | _ -> false
    ) phi
  in
  not @@ List.is_empty uids

let get_structs phi =
  SL.select_subformulae SL.is_spatial_atom phi
  |> List.concat_map (fun psi -> match SL.view psi with
       | PointsTo (_, s, _) -> [s]
       | Predicate (pred, _, _) -> SID.get_structs pred
       | _ -> []
     )
  |> BatList.unique_cmp ~cmp:MemoryModel.StructDef.compare
