module TVL = ThreeValuedLogic
open ThreeValuedLogic

module Logger = Logger.Make(struct
  let name = "Incremental unfolding"
  let level = 1
  let dirname = "footprints"
end)

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
    (* TODO *)
    let atoms = match SL.view phi with
      | Star psis -> List.filter SL.is_atom psis
      | Eq _ | Distinct _ | PointsTo _ -> [phi]
      | Exists (xs, body) -> [] (* TODO: check *)
    in
    let pure, spatial = List.partition SL.is_pure atoms in

    List.filter (SL.is_ground' ~forbidden:(S.elements existentials)) pure
    |> List.map (SL.translate_pure_with_heap_term (Translation.translate_term ctx))
    |> SMT.Boolean.mk_and

  (** Predicate unfolding

      @param existentials     Existential variables introduced during the unfolding process.
      @param must_allocated   Variables set to be must-allocated at the top level unfolding.
      @param allocated        Terms representing allocated locations collected during unfolding. *)
  let rec unfold_pred ~existentials ~must_allocated ~(allocated : SL.Term.t list) ctx n sid pred xs =
    (* TODO: improve for non-empty base case *)
    if n = 0 then InductiveDefinition.unfold_finite pred xs
    else
      let def = InductiveDefinition.instantiate ~refresh:true pred xs in
      let continue_branch guard branch alloc_plus =
        Backend.push guard;
        let allocated = allocated @ alloc_plus in
        let res = unfold_rec ~existentials ~must_allocated ~allocated ctx (n-1) sid branch in
        Backend.pop 1;
       res
      in
      match SL.view def with
      | Exists (xs, body) ->
        (* Just collects existentials and continue without decreasing [n]
           as nothing was unfolded. *)
        let existentials = S.union (S.of_list xs) existentials in
        unfold_rec ~existentials ~must_allocated ~allocated ctx n sid body

      | Ite (cond, t_branch, e_branch) ->
        (* Here, we assume that existential variables are never used in ite-conditions *)
        assert (SL.is_ground' cond ~forbidden:(S.elements existentials));

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
        end

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
      | _ -> unfold_rec ~existentials ~must_allocated ~allocated ctx n sid def

    and unfold_rec ~existentials ~must_allocated ~allocated ctx n sid phi =
      SL.map_view (function
        | Predicate (name, xs, []) when not @@ SID.is_builtin name ->
          let id = SID.get_definition name in
          `Modify (unfold_pred ~existentials ~must_allocated ~allocated ctx n sid id xs)
        | _ -> `Skip
      ) phi

    let unfold_toplevel ctx bound sid phi =
      SL.map_view (function
        | Predicate (name, xs, []) when not @@ SID.is_builtin name ->
          let id = SID.get_definition name in
          (* TODO: unsound, check FP *)
          `Modify (unfold_pred ~existentials:S.empty ~must_allocated:[] ~allocated:[] ctx bound sid id xs)
        | _ -> `Skip
      ) phi

    let unfold input lhs rhs =
      let module C = Translation_context.Make(Encoding.Locations)(Encoding.HeapEncoding) in
      Profiler.add "Unfolding";
      let ctx = C.init input in
      let bound = LocationBounds.sum input.location_bounds - 1 in (* For nil *)
      let sid = SID.id_map () in

      Backend.push lhs; (* TODO: could axioms help? *)

      let res = match Backend.check_sat lhs with
        | SMT_Unsat _ ->
          Logger.debug "LHS is UNSAT\n";
          SL.tt
        | _ -> unfold_toplevel ctx bound sid rhs
      in

      Backend.pop 1;
      Logger.debug "Performed %d SMT queries (%d)\n" (QueryCounter.get ()) (SL.size res);
      res

end
