(* Synthesis result.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

type t = {
  precision : float;
  recall : float;
  concrete_vars : SL.Variable.t list;
  predicates : InductiveDefinition.t list;
}

let is_precise solution = not (solution.precision <> 1.0 || solution.recall <> 1.0)

let canonic_var_names =
  let cnt = ref 0 in
  let rename arity var =
    let old_name = SL.Variable.show var in
    let n = String.drop_first 1 old_name |> int_of_string in
    let new_name =
      if n >= arity (* String.includes ~affix:"!" old_name*)
      then let () = incr cnt in Format.asprintf "E%d" !cnt
      else old_name
    in
    SL.Variable.mk new_name (SL.Variable.get_sort var)
  in
  let apply_formula n = SL.map_view (function
    | Exists (vars, phi) ->
      let vars' = List.map (rename n) vars in
      let by = List.map SL.Term.of_var vars' in
      let phi' = SL.substitute_list ~vars ~by phi in
      `Modify (SL.mk_exists vars' phi')
    | _ -> `Skip)
  in
  let apply_pred pred =
    let n = InductiveDefinition.arity pred in
    InductiveDefinition.map_cases (apply_formula n) pred
  in
  fun self ->
    cnt := 0;
    {self with predicates = List.map apply_pred self.predicates}
