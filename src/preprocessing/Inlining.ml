(* Inlining of trivial inductive definitions.
 *
 * TODO: Relax inlining conditions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module Logger = Logger.Make (struct let name = "Inlining" let level = 2 end)

let can_be_inlined name =
  try
    let id = GlobalSID.find_user_defined name in
    let cases = InductiveDefinition.cases id in
    begin match cases with
    | [case] ->
      (* TODO: Avoid this by doing preprocessing of formula and IDs simultaneously. *)
      let case =
        PreciseToImprecise.to_precise case
        |> QuantifierElimination.apply SL_graph.empty
      in
      Logger.debug "Checking %s (qf: %b, sh: %b, not self-recursive: %b)\n"
        (SL.show case)
        (SL.is_quantifier_free case)
        (SL.is_symbolic_heap case)
        (not @@ GlobalSID.is_self_recursive id.name);
      SL.is_quantifier_free case
      && SL.is_symbolic_heap case
      && not @@ GlobalSID.is_self_recursive id.name

      (* Currently, multiple cases can be inlined only when all of them are atomic *)
      | cases -> List.for_all SL.is_atomic cases
  end
  with Not_found -> false

let inline name xs =
  Logger.debug "Inlining predicate %s(%s)\n" name (SL.Term.show_list xs);
  let id = GlobalSID.find_user_defined name in
  InductiveDefinition.instantiate ~refresh:true id xs
  (* TODO: Avoid this by doing preprocessing of formula and IDs simultaneously. *)
  |> PreciseToImprecise.to_precise
  |> QuantifierElimination.apply SL_graph.empty

let inline phi =
  SL.map_view (function
    | Predicate (name, xs, []) when GlobalSID.is_user_defined name && can_be_inlined name ->
      `Modify (inline name xs)
    | _ -> `Skip
  ) phi

let inline phi =
  Logger.debug "Running inlining\n";
  let phi' = inline phi in
  if SL.equal phi phi' then phi
  else inline phi'

let inline_ctx ctx =
  let open Context in
  {ctx with phi = inline ctx.phi}
