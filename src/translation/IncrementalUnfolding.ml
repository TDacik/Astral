(* Incremental unfolding of entailment RHS using incremental SMT solving.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module TVL = ThreeValuedLogic
open ThreeValuedLogic

module Logger = Logger.Make(struct
  let name = "Incremental unfolding"
  let level = 1
end)

let compute_lookahaed sl_graph ground var =
  let g = SL_graph.projection_pointer sl_graph in
  let target = SL_graph.find_reachable g var ground in
  match target with
    | None -> None
    | Some target ->
      let path = SL_graph.find_path g var target in
      Logger.debug "Look-ahead: %s -[%s]-> %s\n"
        (SL.Term.show var) (MemoryModel.Field.show_list path) (SL.Term.show target);
      Some (path, target)

module Make (Encoding : Translation_sig.ENCODING) (Backend : Backend_sig.BACKEND) = struct

  module S = SL.Variable.Set

  module Translation = Translation.Make(Encoding)(Backend)
  module QueryCounter = Counter.Simple ()
  module FpCounter = Counter.Simple ()

  let check_simple cond =
    QueryCounter.inc ();
    match Backend.check_sat cond with
      | SMT_Unsat _ -> False
      | _ -> Unknown

  let check cond =
    match check_simple cond with
      | False -> False
      | Unknown -> begin match check_simple @@ SMT.Boolean.mk_not cond with
        | False -> True
        | _ -> Unknown
      end

  (* TODO: can be improved *)
  let abstraction ctx existentials phi =
    let atoms = match SL.view phi with
      | Star psis -> List.filter SL.is_atom psis
      | Eq _ | Distinct _ | PointsTo _ -> [phi]
      | Exists (xs, body) -> [] (* TODO: check *)
    in
    let pure, spatial = List.partition SL.is_pure atoms in

    List.filter (SL.is_ground' ~forbidden:(S.elements existentials)) pure
    |> List.map (SL.translate_pure_with_heap_term (Translation.translate_term ctx))
    |> SMT.Boolean.mk_and

  let apply_lookahead (cond : SL.t) to_remove lookaheads =
    let atoms = match SL.view cond with SL.And xs -> xs | _ when SL.is_atom cond -> [cond] in
    let res = List.map (fun atom -> match SL.view atom with
        | SL.Eq [y; x] when SL.Term.MonoList.mem x to_remove ->
          let n = Option.get @@ List.find_index (SL.Term.equal x) to_remove in
          let path, target = List.nth lookaheads n in
          let move = List.fold_left (fun acc f -> SL.Term.mk_heap_term f acc) y path in
          let res = SL.mk_eq2 move target in
          let hint = SL.mk_eq2 x y in
          (res, hint)
        | SL.Eq _ -> failwith "TODO"
        | _ -> assert false
    ) atoms
    in
    SL.mk_and @@ List.map fst res, SL.mk_and @@ List.map snd res

  (** Predicate unfolding

      @param existentials     Existential variables introduced during the unfolding process. *)
  let rec unfold_pred ~existentials ctx sl_graph n sid pred xs =
    (* TODO: improve for non-empty base case *)
    if n = 0 then InductiveDefinition.unfold_finite pred xs
    else
      let def = InductiveDefinition.instantiate ~refresh:true pred xs in
      let continue_branch guard branch alloc_plus =
        Backend.push guard;
        let res = unfold_rec ~existentials ctx sl_graph (n-1) sid branch in
        Backend.pop 1;
       res
      in
      match SL.view def with
      | Exists (xs, body) ->
        (* Just collects existentials and continue without decreasing [n]
           as nothing was unfolded. *)
        let existentials = S.union (S.of_list xs) existentials in
        unfold_rec ~existentials ctx sl_graph n sid body

      | Ite (cond, t_branch, e_branch) ->
        if SL.is_ground' cond ~forbidden:(S.elements existentials) then (
          let c = SL.translate_pure_with_heap_term (Translation.translate_term ctx) cond in
          let res = check c in
          Logger.debug "[|%s|] -> %s\n" (SMT.show c) (TVL.show res);

          (* TODO: why no alloc_plus? *)
          let continue_t () = continue_branch c t_branch [] in

          let alloc_plus = SL_graph.must_alloc @@ SL_graph.compute def in
          let continue_e () = continue_branch (SMT.Boolean.mk_not c) e_branch alloc_plus in

          begin match res with
            | True -> continue_t ()
            | False -> continue_e ()
            | Unknown -> SL.mk_ite cond (continue_t ()) (continue_e ())
          end)
        else if Options.unfolding_lookahead () then (
          (* Remove existentials by look-ahaed *)
          let existentials = List.map SL.Term.of_var @@ S.elements existentials in
          let ground = List.map SL.Term.of_var @@ SL.free_vars ctx.phi in
          let to_remove = SL.Term.MonoList.inter xs existentials in
          let lookaheads = List.map (compute_lookahaed sl_graph ground) to_remove in
          if List.for_all Option.is_some lookaheads then
            let lookaheads = List.map Option.get lookaheads in
            let c0, hint = apply_lookahead cond to_remove lookaheads in
            let c = SL.translate_pure_with_heap_term (Translation.translate_term ctx) c0 in
            let res = check c in
            Logger.debug "[|%s|] -> %s\n" (SMT.show c) (TVL.show res);

            (* TODO: why no alloc_plus? *)
            let continue_t () = continue_branch c t_branch [] in

            let alloc_plus = SL_graph.must_alloc @@ SL_graph.compute def in
            let continue_e () = continue_branch (SMT.Boolean.mk_not c) e_branch alloc_plus in

            begin match res with
              | True -> SL.mk_and [continue_t (); hint]
              | False -> continue_e ()
              | Unknown -> SL.mk_ite c0 (SL.mk_and [continue_t (); hint]) (continue_e ())
            end

          (* Otherwise, do full unfolding *)
          else InductiveDefinition.unfold sid pred xs n
        )
        (* Otherwise, do full unfolding *)
        else InductiveDefinition.unfold sid pred xs n
      | Or cases ->
        (* Continue by only those cases that are feasible on the left-hand side. *)
        List.fold_left (fun acc case ->
          let cond = abstraction ctx existentials case in
          let res = check_simple cond in
          Logger.debug "OR: [|%s|] -> %s\n" (SMT.show cond) (TVL.show res);

          begin match res with
            | False -> acc (* Case is infeasible *)
            | Unknown ->
              let alloc_plus = SL_graph.must_alloc @@ SL_graph.compute case in
              let case' = continue_branch cond case alloc_plus in
              SL.mk_or [acc; case']
          end
        ) SL.ff cases

      (* Non-disjunctive definition *)
      | _ -> unfold_rec ~existentials ctx sl_graph n sid def

    and unfold_rec ~existentials ctx sl_graph n sid phi =
      SL.map_view (function
        | Predicate (name, xs, _) when SID.is_user_defined name ->
          let id = SID.get_definition name in
          `Modify (unfold_pred ~existentials ctx sl_graph n sid id xs)
        | _ -> `Skip
      ) phi

    let unfold_toplevel ~existentials ctx sl_graph bound sid phi =
      SL.map_view (function
        | Predicate (name, xs, _) when SID.is_user_defined name ->
          let id = SID.get_definition name in
          (* TODO: unsound, check FP *)
          `Modify (unfold_pred ~existentials ctx sl_graph bound sid id xs)
        | _ -> `Skip
      ) phi

    let unfold input lhs rhs =
      let module C = Translation_context.Make(Encoding.Locations)(Encoding.HeapEncoding) in
      Profiler.add "Unfolding";

      Backend.init ~timeout:(Options.incremental_timeout ()) ();

      (* Is this sound??? *)
      let sl_graph = SL_graph.compute rhs in

      let ctx = C.init input in
      let bound = LocationBounds.sum input.location_bounds - 1 in (* -1 for nil *)
      let sid = SID.id_map () in

      Backend.push lhs; (* TODO: could adding axioms help? *)

      let res = match Backend.check_sat lhs with
        | SMT_Unsat _ -> Logger.debug "LHS is UNSAT\n"; SL.tt
        | _ ->
          let existentials = S.of_list @@ SL.bound_vars rhs in
          unfold_toplevel ~existentials ctx sl_graph bound sid rhs
      in

      Backend.pop 1;
      Logger.debug "Performed %d SMT queries\n" (QueryCounter.get ());
      res

end
