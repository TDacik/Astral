
module Input = Context
module Context = Translation_context

open ThreeValuedLogic
open InductiveDefinition

module Logger = Logger.Make(struct let name = "Incremental unfolding" let level = 1 end)

module Make (Encoding : Translation_sig.ENCODING) (Backend : Backend_sig.BACKEND) = struct

  module Translation = Translation.Make(Encoding)(Backend)
  module Footprints = FootprintEncoding.Make(Encoding)(Backend)

  let cnt = ref 0

  let check_simple cond =
    match incr cnt; Backend.check_sat cond with
      | SMT_Unsat _ -> False
      | _ -> Unknown

  let check cond =
    match incr cnt; Backend.check_sat cond with
      | SMT_Unsat _ -> False
      | _ ->
        begin match incr cnt; Backend.check_sat @@ SMT.Boolean.mk_not cond with
          | SMT_Unsat _ -> True
          | _ -> Unknown
        end

  let lhs_t = ref SMT.Boolean.tt
  let cache = ref SL.Map.empty

  let get_root atom = match SL.view atom with
    | SL.Emp -> []
    | SL.PointsTo (x, _, _) -> [x]
    | SL.Predicate (name, params, _) ->
      let abstraction = SID.abstraction name in
      [PredicateAbstraction.get_root abstraction ~params]
    | _ -> failwith @@ SL.show atom

  let pure_abstraction ctx phi =
    assert (SL.is_symbolic_heap phi);
    let _, atoms = SL.as_quantified_symbolic_heap phi in
    let pure, spatial = List.partition SL.is_pure atoms in

    let pure = List.map (SL.translate_pure_with_heap_term (Translation.translate_term ctx)) pure in

    let roots =
      List.concat_map get_root spatial
      |> List.map (Translation.translate_term ctx)
    in
    SMT.Boolean.mk_and pure

    (*
    let p = SMT.Boolean.mk_and [SMT.Boolean.mk_and pure; SMT.Boolean.mk_distinct roots] in
    SMT.print ~prefix:"Abstraction:" p;
    p
    *)

  let has_unique_footprint abs id =
    let may_allocated = PredicateAbstraction.may_allocated abs in
    let res = List.is_empty may_allocated || InductiveDefinition.is_ite id in
    Logger.debug "UFP: %b\n" res;
    res

  let rec unfold_step ctx n ?(boundaries=[]) ~allocated ~existential id id_map name xs =
    Logger.debug "Remaining: %d\n" n;
    if n = 0 then unfold_finite id xs
    else
      let def = instantiate ~refresh:true id xs in
      match SL.view def with
      (* TODO: existential prefix for ite*)
      | Ite (cond, t, e) ->
        let c = SL.translate_pure_with_heap_term (Translation.translate_term ctx) cond in
        (*let c = SMT.Quantifier.mk_exists existential c in *)
        let res = check c in
        Logger.debug "%s --> %s\n" (SMT.show c) (ThreeValuedLogic.show res);

        let continue_t () =
          Backend.push c;
          (*Backend.push @@ pure_abstraction ctx t;*)
          let existential = existential @ (List.map (Translation.translate_var ctx) (SL.bound_vars t)) in
          let res = unfold_predicate ~existential ~boundaries ~first:false (n - 1) ctx id_map t in
          Backend.pop 1;
          res
        in

        let continue_e () =
          Backend.push (SMT.Boolean.mk_not c);
          (*Backend.push @@ pure_abstraction ctx e;*)
          let existential = existential @ (List.map (Translation.translate_var ctx) (SL.bound_vars e)) in
          let res = unfold_predicate ~existential ~boundaries ~first:false (n - 1) ctx id_map e in
          Backend.pop 1;
          res
        in
        begin match res with
          | True -> continue_t ()
          | False -> continue_e ()
          | Unknown -> SL.mk_ite cond (continue_t ()) (continue_e ())
        end
      | Or cases ->
        List.fold_left (fun acc case ->
          let es, atoms = SL.as_quantified_symbolic_heap case in
          let pure, spatial = List.partition SL.is_pure atoms in
          let c = List.map (SL.translate_pure_with_heap_term (Translation.translate_term ctx)) pure in
          let c = pure_abstraction ctx case in
          let check = check_simple c in
          Logger.debug "%s --> %s\n" (SMT.show c) (ThreeValuedLogic.show check);
          let res = match check with
            | False -> acc
            | _ ->
              let allocated =
                List.filter SL.is_pointer spatial
                |> List.map SL.as_pointer
                |> List.map (fun (x, _, _) -> x)
                |> List.append allocated
              in
              SL.mk_or [acc; (Backend.push c; let res = unfold_predicate (n-1) ~allocated ~boundaries ~first:false ctx id_map case in Backend.pop 1; res)]
          in
          (if Options_base.fp_construction ()
           then
             cache := SL.Map.add res (Footprints.mk_footprint ctx ~allocated ~boundaries !lhs_t id xs) !cache
           else ()
          );
          res
          ) SL.ff cases

      (* Non-disjunctive definition *)
      | _ -> unfold_predicate n ~allocated ~boundaries ~first:false ctx id_map def

  (** Unfolding of an inductive predicate.

      - If the predicate has non-unique footprint, create a single case-split at the
        top-level and continue with unique footprints given by fixed may-allocated
        parameters. *)
  and unfold_predicate n ?(allocated=[]) ?(boundaries=[]) ?(existential=[]) ?(first=true) ctx id_map phi =
    SL.map_view (function
      | Predicate (name, xs, _) when not @@ SID.is_builtin name ->
        let id = SID.get_definition name in
        let abs = SID.abstraction name in
        Logger.debug "UFPPP: %b\n" (has_unique_footprint abs id);
        `Modify (unfold_step ctx n ~allocated ~boundaries ~existential id id_map name xs)
        (*let result =
          if has_unique_footprint abs id || not first
          then unfold_step ctx n ~allocated ~boundaries ~existential id id_map name xs
          else
            let root = PredicateAbstraction.get_root abs ~params:xs in
            let may_allocated = PredicateAbstraction.may_allocated abs ~params:xs in
            let cases = List_utils.sublists may_allocated in
            let res = SL.mk_or @@ List.map (fun never_allocated ->
              let module S = SL.Term.Set in
              let must_allocated = S.elements @@ S.diff (S.of_list may_allocated) (S.of_list never_allocated) in
              let never = List.map (Translation.translate_term ctx) never_allocated in
              let must = List.map (Translation.translate_term ctx) must_allocated in
              let dom = Translation.formula_footprint ctx (SL.mk_predicate name xs) in (* TODO: check *)
              let axioms =
                SL.mk_and [(*
                  SL.mk_pure @@ SMT.Sets.mk_subset (SMT.Sets.mk_constant ctx.fp_sort must) dom;
                  SL.mk_pure @@ SMT.Sets.mk_disjoint[SMT.Sets.mk_constant ctx.fp_sort never; dom];*)
                  unfold_step ctx n ~allocated ~boundaries:never_allocated ~existential id id_map name xs
                ]
              in
              axioms
              ) cases
            in
            (*
            let fps =
              List.map (List.map (Translation.translate_term ctx)) cases
              |> List.map (fun c -> SMT.Sets.mk_constant ctx.fp_sort c)
            in
            cache := PrecomputedFootprints.add res fps !cache;
            Logger.dump ~filename:("toplevel_" ^ Int.to_string @@ FpCounter.next ()) (phi, res, fps);
            *)
            `Modify res*)
          (*else
            `Modify (unfold_pred ~existentials:S.empty ~must_allocated:[] ~allocated:[] ctx bound sid id xs)
          *)
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
