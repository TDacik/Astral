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
  (* Create fresh predicate interface *)
  let header' = List.map SL.Variable.refresh header in
  let def = SL.substitute_list def ~vars:header ~by:(List.map SL.Term.of_var header') in

  let base, inductive = match SL.view def with
    | Or xs -> List.partition SL.is_atomic xs
    | _ when SL.is_atomic def -> [def], []
    | _ -> [], [def]
  in
  {name = name; header = header'; base_cases = base; inductive_cases = inductive}

let arity self = List.length self.header

(** Refresh existential variables for unfolding. *)
let refresh id =
  let aux = SL.map_view (function
  | Exists (vs, body) ->
    let vs' = List.map (fun x -> SL.Variable.refresh x) vs in
    let body' = SL.substitute_list body ~vars:vs ~by:(List.map SL.Term.of_var vs') in
    SL.mk_exists vs' body'
  )
  in
  {id with base_cases = List.map aux id.base_cases; inductive_cases = List.map aux id.inductive_cases}

let refresh_fn = refresh

let instantiate ~refresh id xs =
  assert (List.compare_lengths xs id.header == 0);
  let id = if refresh then refresh_fn id else id in
  let phi = SL.mk_or (id.base_cases @ id.inductive_cases) in
  SL.substitute_list phi ~vars:id.header ~by:xs

let instantiate_formals ?(refresh=false) id =
  let id = if refresh then refresh_fn id else id in
  instantiate ~refresh id @@ List.map SL.Term.of_var id.header

let dependencies id =
  id.inductive_cases
  |> List.concat_map (SL.select_subformulae SL.is_predicate)
  |> List.map (fun psi -> match SL.view psi with Predicate (name, _, _) -> name)

let fields id = SL.get_fields @@ instantiate_formals id

let has_base_cases id = not @@ List.is_empty id.base_cases

let cases id = id.base_cases @ id.inductive_cases

let map fn id = mk id.name id.header (fn @@ instantiate_formals id)

let map_cases fn id =
  {id with
    base_cases = List.map fn id.base_cases;
    inductive_cases = List.map fn id.inductive_cases
  }


(* SUBSTITUTE:
let instantiate id xs =
  let subst x = SL.substitute_list ~vars:id.header ~by:xs x in
  let base = List.map subst id.base_cases in
  let inductive = List.map subst id.inductive_cases in
  {id with base_cases = base; inductive_cases = inductive}
*)


(** {2 Finite IDs} *)

let is_finite id = List.is_empty id.inductive_cases

let unfold_finite id xs : SL.t =
  let phi = SL.mk_or id.base_cases in
  SL.substitute_list phi ~vars:id.header ~by:xs

(** {2 Unfolding of IDs *)
