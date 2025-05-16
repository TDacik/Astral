(* Simplification rules that do not preserve models
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module Logger = Logger.Make (struct
    let name = "Simplifier 2"
    let level = 2
  end)


(** TODO: implement general way to check wether predicate is guaranteed
          to be non-empty. *)
let get_roots phi =
  let get_root psi = match SL.view psi with
    | PointsTo (x, _, _) -> Some x
    | Predicate ("ls", [x; y], _) when not @@ SL.Term.equal x y -> Some x
    | _ -> None
  in
  SL.select_subformulae SL.is_spatial_atom phi
  |> List.filter_map get_root

let apply phi = match SL.as_query phi with
  | SL.SymbolicHeap_ENTL (lhs, rhs) ->
    let module S = SL.Term.Set in
    let lhs_vars = List.map SL.Term.of_var @@ SL.free_vars lhs in
    let rhs_vars = get_roots rhs in
    if S.subset (S.of_list rhs_vars) (S.of_list lhs_vars) then phi
    else
      let _ = Logger.debug "Reducing ENTL to SAT of lhs\n" in
      lhs
  | _ -> phi

let apply_ctx ctx =
  Context.{ctx with phi = apply ctx.phi}
