type t = {
  name : string;
  header : SL.Variable.t list;

  base_cases : SL.t list;
  inductive_cases : SL.t list;
}

let name self = self.name

let header self = self.header

let show id =
  Format.asprintf "%s(%s) :=\n  %s"
    (id.name)
    (String.concat ", " @@ List.map SL.Variable.show id.header)
    (String.concat "\n  | " @@ List.map SL.show @@ id.base_cases @ id.inductive_cases)

let compare id1 id2 = String.compare id1.name id2.name

module Self = struct
  type nonrec t = t
  let show = show
  let compare = compare
end

include Datatype.Printable(Self)
include Datatype.Collections(Self)


let equal id1 id2 = String.equal id1.name id2.name

let hash id = String.hash id.name

let mk_call id = SL.mk_predicate (name id) (List.map SL.Term.of_var id.header)

let mk name header def =
  let base, inductive = match SL.view def with
    | Or xs -> List.partition SL.is_atomic xs
    | _ when SL.is_atomic def -> [def], []
    | _ -> [], [def]
  in
  {name = name; header = header; base_cases = base; inductive_cases = inductive}

let arity self = List.length self.header

(** Replace input variables by fresh ones. This needs to be done to prevent variable capture
    as in the following case:  def = ls(x, y)    and    phi = ls(y, nil) ~~> ls(nil, nil) *)
let refresh_header id =
  let fresh_header = List.map SL.Variable.refresh id.header in
  let substitute = SL.substitute_list ~vars:id.header ~by:(List.map SL.Term.of_var fresh_header) in
  {
    id with header = fresh_header;
           base_cases = List.map substitute id.base_cases;
           inductive_cases = List.map substitute id.inductive_cases;
  }

(** Refresh existential variables for unfolding. *)
let refresh_existentials id =
  let aux = SL.map_view (function
  | Exists (vs, body) ->
    let vs' = List.map (fun x -> SL.Variable.refresh x) vs in
    let body' = SL.substitute_list body ~vars:vs ~by:(List.map SL.Term.of_var vs') in
    SL.mk_exists vs' body'
  )
  in
  {id with base_cases = List.map aux id.base_cases; inductive_cases = List.map aux id.inductive_cases}

let to_formula ?params id = match params with
  | None -> SL.mk_predicate id.name (List.map SL.Term.of_var id.header)
  | Some xs -> SL.mk_predicate id.name xs

let instantiate ~refresh id xs =
  assert (List.compare_lengths xs id.header == 0);
  let id =
    if refresh
    then refresh_header @@ refresh_existentials id
    else id
  in
  let phi = SL.mk_or (id.base_cases @ id.inductive_cases) in
  SL.substitute_list phi ~vars:id.header ~by:xs

let instantiate_formals ?(refresh=false) id =
  let id = if refresh then refresh_existentials id else id in
  instantiate ~refresh id @@ List.map SL.Term.of_var id.header

let dependencies id =
  id.inductive_cases
  |> List.concat_map (SL.select_subformulae SL.is_predicate)
  |> List.map (fun psi -> match SL.view psi with Predicate (name, _, _) -> name)

let fields id = SL.get_fields @@ instantiate_formals id

let has_base_cases id = not @@ List.is_empty id.base_cases

let cases ?(refresh=true) ?params id =
  let id = if refresh then refresh_existentials id else id in
  match params with
  | None -> id.base_cases @ id.inductive_cases
  | Some params ->
    id.base_cases @ id.inductive_cases
    |> List.map (SL.substitute_list ~vars:id.header ~by:params)

let map fn id = mk id.name id.header (fn @@ instantiate_formals id)

let map_cases fn id =
  {id with
    base_cases = List.map fn id.base_cases;
    inductive_cases = List.map fn id.inductive_cases
  }

(** {2 Finite IDs} *)

let is_finite id = List.is_empty id.inductive_cases

let unfold_finite id xs : SL.t =
  let phi = SL.mk_or id.base_cases in
  SL.substitute_list phi ~vars:id.header ~by:xs
