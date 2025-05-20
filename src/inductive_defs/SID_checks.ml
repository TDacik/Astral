(* Tests whether SID satisfy conditions that guarantee existence of distinguishing models.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

type distinguisher =
  | Sort
  | Field
  | No

module Logger = Logger.Make(struct
  let name = "SID checker"
  let level = 2
end)

module M = struct
  include Map.Make
    (struct
      type t = InductiveDefinition.t * InductiveDefinition.t
        [@@deriving compare]
      let compare id1 id2 =
        match compare id1 id2, compare id2 id1 with
          | 0, _ | _, 0 -> 0
          | x, _ -> x
    end)
end

type t = distinguisher M.t

let is_distinguishable_by_sort id =
  M.for_all (fun (id1, id2) d ->
    if InductiveDefinition.equal id id1 || InductiveDefinition.equal id id2 then d = Sort
    else true
  )

(** Check whether two inductive definitions are indistinguishable by sorts *)
let check_sort root1 root2 =
  not @@ Sort.equal (SL.Variable.get_sort root1) (SL.Variable.get_sort root2)

let check_field_case root field case =
  let _, atoms = SL.as_quantified_symbolic_heap case in
  let ptr =
    List.find_opt (fun atom -> match SL.view atom with
      | PointsTo (x, _, _) when SL.Variable.equal (SL.Term.as_var x) root -> true
      | _ -> false
    ) atoms
  in
  match ptr with
    | None -> false
    | Some ptr ->
      let _, def, ys = SL.as_pointer ptr in
      let y = MemoryModel.StructDef.field_value def field ys in
      SL.Term.is_nil y

let check_field_case2 root field case =
  let _, atoms = SL.as_quantified_symbolic_heap case in
  let ptr =
    List.find_opt (fun atom -> match SL.view atom with
      | PointsTo (x, _, _) when SL.Variable.equal (SL.Term.as_var x) root -> true
      | _ -> false
    ) atoms
  in
  match ptr with
    | None -> false
    | Some ptr ->
      let _, def, ys = SL.as_pointer ptr in
      let y = MemoryModel.StructDef.field_value def field ys in
      not @@ SL.Term.is_nil y


let check_field id1 id2 root1 root2 =
  let check id id' root root' =
    let cases = List.filter (fun phi -> not @@ SL.is_pure phi) @@ InductiveDefinition.cases id in
    let field = SL.get_fields @@ InductiveDefinition.instantiate_formals id' in
    match List.find_opt (fun f -> List.for_all (check_field_case root f) cases) field with
      | None -> false
      | Some field ->
        let cases2 = InductiveDefinition.cases id' in
        List.exists (check_field_case2 root' field) cases2
  in
  check id1 id2 root1 root2 || check id2 id1 root2 root1


let check_id_pair res (id1, id2) =
  let open InductiveDefinition in
  let root1 = List.hd id1.header in (* TODO *)
  let root2 = List.hd id2.header in (* TODO *)
  if check_sort root1 root2 then
    let _ = Logger.debug "Predicates %s and %s can be distinguished by sort\n" id1.name id2.name in
    M.add (id1, id2) Sort res
  else if check_field id1 id2 root1 root2 then
    let _ = Logger.debug "Predicates %s and %s can be distinguished by field\n" id1.name id2.name in
    M.add (id1, id2) Field res
  else
    M.add (id1, id2) No res
    (*Result.error
    @@ Format.asprintf "Inductive definitions %s and %s are not distinguishable"
        (InductiveDefinition.name id1)
        (InductiveDefinition.name id2)
      *)

let compute_distinguishers () = M.empty
(*
  SID0.get_user_defined ()
  |> List_utils.diagonal_product
  |> List.fold_left check_id_pair M.empty *)
