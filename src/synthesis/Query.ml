(* Representation of single query for synthesis.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

type t = {
  vars : SL.Variable.t list;
  examples : SL.t list;
  existentials : SL.Variable.t list;
}

let mk vars existentials examples =
  {vars; examples; existentials}

let root q = List.hd q.vars

let not_garbage query =
  List.exists (fun e ->
    let [], atoms = SL.as_quantified_symbolic_heap e in
    not @@ List.for_all SL.is_pure atoms
  ) query.examples

let is_simple query = match query.examples with
  | [e] -> (
    let [], atoms = SL.as_quantified_symbolic_heap e in
    if List.for_all SL.is_pure atoms then
      Logging.warning "Silently droping synthesis query withou spatial atoms";
      true (* TODO: should not happen *)
    else
      let atoms = List.filter SL.is_spatial_atom atoms in
      match atoms with
      | [a] -> (
        let vars = match SL.view a with
          | PointsTo (x, _, ys) -> x :: ys
          | Predicate (_, xs, _, _) -> xs
        in
        let existentials = List.map SL.Term.of_var query.existentials in
        SL.Term.MonoList.are_disjoint existentials vars
      )
      | _ -> false)
  | _ -> false
