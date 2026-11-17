(* Mapping of predicates to models.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

let rec instance_generator n xs =
  if n = 0 then Seq.return []
  else
    let tail = instance_generator (n-1) xs in
    Seq.concat_map (fun x -> Seq.map (fun s -> x :: s) tail) (List.to_seq xs)

let check_entl ctx rhs lhs =
  let phi = SL.mk_gneg lhs rhs in
  let res = Engine.solve {ctx with assertions = [phi]; produce_models = false} in
  let res = match res.status with
    | Some (`Unsat) -> true
    | Some (`Sat) -> false
    | Some (`Unknown reason) -> failwith reason
  in
  (*Format.printf "%s |= %s: %b\n" (SL.show lhs) (SL.show rhs) res;*)
  res

  (*match ModelChecker.check sh phi with
  | Ok res -> res
  | Error (Unsupported msg) -> failwith msg
  | Error (Failure (e, _)) -> raise e*)

let check_instance ctx id params =
  let phi = InductiveDefinition.to_formula id ~params in
  if List.for_all (check_entl ctx phi) ctx.positive_examples
  then Some phi
  else None


let match_predicate ctx id =
  let xs = SL.Term.nil :: (List.map SL.Term.of_var @@ ParserContext.get_sl_vars ctx) in
  let arity = InductiveDefinition.arity id in
  let instances = instance_generator arity xs in
  Seq.find_map (check_instance ctx id) instances

let mmatch ctx : SL.t option =
  GlobalSID.fold_user_defined (fun pred acc ->
    match acc with
    | None -> match_predicate ctx pred
    | Some _ -> acc
  ) None
