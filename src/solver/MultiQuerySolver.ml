(* TODO
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

open Context
open Backend_sig
open Translation_sig

module Make (Encoding : ENCODING) (Backend : BACKEND) = struct

  module Translation = Translation.Make(Encoding)(Backend)

  type t = Model of Context.t | Exhausted

  (** Single query. We need to instruct unfolding to use fixed bound. *)
  let single_query ctx case bound_map =
    let sl_graph = SL_graph.compute case in
    let ctx' = {ctx with phi = case; sl_graph} in
    Preprocessor.third_phase ~bound_map ctx'
    |> Translation.solve

  let solve_cases = failwith "TODO" (*function
    | [] -> Exhausted
    | (case, bounds) :: rest ->
      let res = single_query case bounds in
      match Option.get result.status with
        | `Sat -> Model res
        | `Unsat -> solve_cases res*)

  let split lhs_predicates 1 = failwith "TODO" (*
    List.map (fun pred ->
      List.map (fun pred' ->
        if SL.equal pred pred' then (pred, 1)
        else (pred, 0)
      ) lhs_predicates
    ) lhs_predicate*)

  let solve context = failwith "TODO"
  (*
    let dangling_terms = SLID.dangling_terms context.phi in
    match dangling_terms with
      | 1 ->
        let cases = split lhs_predicates 1 in
        solve_cases cases
      | _ -> SingleQuery.solve context
*)
end
