(* Operations over system of inductive definitions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open ID_sig

(** This module provies unified access to both built-in and user-defined predicates. *)
module ID = struct

  type t =
    | Builtin of (module BUILTIN)
    | UserDefined of InductiveDefinition.t

  let name = function
    | Builtin (module B : BUILTIN) -> B.name
    | UserDefined id -> InductiveDefinition.show id

  let show pred =
    let kind = match pred with
      | Builtin _ -> "built-in"
      | UserDefined _ -> "user defined"
    in
    Format.asprintf "%s (%s)" (name pred) kind

  let to_id = function
    | Builtin (module B : BUILTIN) ->
      let xs = List.map (fun sort -> SL.Variable.mk_fresh "x" sort) B.signature in
      InductiveDefinition.mk B.name xs @@ SL.mk_or @@ B.rules (xs, B.default_instantiation)
    | UserDefined id -> id

end

open ID


(** System of inductive definitions maps predicate identifiers to
    their definitions (either builtin or user defined. *)
module M = Stdlib.Map.Make(String)

type t = {
  definitions : ID.t M.t;
  graph : DependencyGraph.t; [@warning "-69"]
}
let empty = {
  definitions = M.empty;
  graph = DependencyGraph.empty;
}

let dependency_graph sid = sid.graph

let register sid name id =
  assert (not @@ M.mem name sid.definitions);
  {sid with definitions = M.add name id sid.definitions}
  (** TODO: recompute graph after every change? *)

let register_builtin sid (module B : BUILTIN) =
  register sid B.name (Builtin (module B))

let register_user_defined sid id = register sid (InductiveDefinition.name id) (UserDefined id)

let update_user_defined sid id =
  let name = InductiveDefinition.name id in
  let sid' = {sid with definitions = M.remove name sid.definitions} in
  register_user_defined sid' id

let show sid =
  M.bindings sid.definitions
  |> List.map (fun (_, pred) -> ID.show pred)
  |> String.concat ",\n"

module Self = struct
  type nonrec t = t
  let show = show
end

include Datatype.Printable(Self)

let mem name sid = M.mem name sid.definitions

let find sid name =
  try M.find name sid.definitions
  with Not_found ->
    Exceptions.internal_error
      ~reason:("No definition for predicate " ^ name)
      ~details:("Registered predicates:\n" ^ show sid)

let find_user_defined sid name = match find sid name with
  | UserDefined id -> id
  | _ ->
    Exceptions.internal_error
      ~reason:("No user-defined definition for predicate " ^ name ^ "(built-in exists)")
      ~details:("Registered predicates:\n" ^ show sid)

let find_builtin sid name = match find sid name with
  | Builtin (module B : BUILTIN) -> (module B : BUILTIN)
  | _ ->
    Exceptions.internal_error
      ~reason:("No user-defined definition for predicate " ^ name ^ "(built-in exists)")
      ~details:("Registered predicates:\n" ^ show sid)

let is_builtin sid name =
  if not @@ mem name sid then false (* TODO: or raise? *)
  else match find sid name with
    | Builtin _ -> true
    | UserDefined _ -> false

let is_user_defined sid name =
  if not @@ mem name sid then false (* TODO: or raise? *)
  else match find sid name with
    | Builtin _ -> false
    | UserDefined _ -> true

(** TODO: modify graph accordingly *)
let filter_map fn sid =
  {sid with definitions = M.filter_map (fun _ pred -> fn pred) sid.definitions}

let fold fn sid acc =
  M.fold (fun _ pred acc -> fn pred acc) sid.definitions acc

let fold_builtin fn sid acc =
  M.fold (fun _ pred acc -> match pred with
    | Builtin (module B : BUILTIN) -> fn (module B : BUILTIN) acc
    | UserDefined _ -> acc
  ) sid.definitions acc

let fold_user_defined fn sid acc =
  M.fold (fun _ pred acc -> match pred with
    | Builtin _ -> acc
    | UserDefined id -> fn id acc
  ) sid.definitions acc

let get_builtin sid = fold_builtin List.cons sid []
let get_user_defined sid = fold_user_defined List.cons sid []

(** ==== Operations over dependency graph ==== *)

(* TODO *)
let dependencies sid name = match find sid name with
  | Builtin _ -> []
  | UserDefined id ->
    InductiveDefinition.dependencies id
    |> List.map (find_user_defined sid)

let is_self_recursive sid name =
  match find sid name with
  | Builtin _ -> true (* Conservatively assume true *)
  | UserDefined id -> DependencyGraph.is_self_recursive sid.graph id

(** ==== Unfolding ==== *)


(** {2 Unfolding of inductive definitions} *)

(** Compute how many locations will the rule allocate. *)
let case_size rule =
  let _, atoms = SL.as_quantified_symbolic_heap rule in
  List.length @@ List.filter SL.is_pointer atoms

let base_size id = match InductiveDefinition.cases ~base_only:true id with
  | [] -> 0 (* TODO: check *)
  | bs -> BatList.min @@ List.map case_size bs

let rec unfold_case sid n case =
  let rest = n - case_size case in
  if rest < 0 then SL.ff
  else SL.map_view (function
    | Predicate (name, ys, _) ->
      `Modify (unfold_id sid name ys rest)
    | _ -> `Skip
  ) case

and unfold_id sid name xs n =
  let id = find_user_defined sid name in
  let cases = InductiveDefinition.instantiate_rules id xs in
  let fn case =
    let _, atoms = SL.as_quantified_symbolic_heap case in
    let malus =
      List.filter SL.is_predicate atoms
      |> List.map SL.as_predicate
      |> List.map (fun (name, _) -> find_user_defined sid name)
      |> List.map base_size
      |> (fun xs -> try List.tl xs with _ -> xs) (* TODO: remove systematically *)
      |> BatList.sum
    in
    unfold_case sid (n - malus) case
  in
  let cases' =
    List.map (fun case -> match SL.view case with
      | Ite (cond, t, e) -> SL.mk_ite cond (fn t) (fn e)
      | _ -> fn case
    ) cases
  in
  SL.mk_or @@ cases'

let unfold = unfold_id
