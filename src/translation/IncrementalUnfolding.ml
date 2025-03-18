
module Input = Context
module Context = Translation_context

open ThreeValuedLogic
open InductiveDefinition

module Logger = Logger.Make(struct let name = "Incremental unfolding" let level = 1 end)

module Make (Encoding : Translation_sig.ENCODING) (Backend : Backend_sig.BACKEND) = struct

  module Translation = Translation.Make(Encoding)(Backend)

  let cnt = ref 0

  let check cond =
    match incr cnt; Backend.check_sat cond with
      | SMT_Unsat _ -> False
      | _ ->
        begin match incr cnt; Backend.check_sat @@ SMT.Boolean.mk_not cond with
          | SMT_Unsat _ -> True
          | _ -> Unknown
        end

  let rec unfold_step ctx n id id_map name xs =
    if n = 0 then unfold_finite id xs
    else
      let def = instantiate ~refresh:true id xs in
      match SL.view def with
      | Ite (cond, t, e) ->
        let c = SL.translate_pure_with_heap_term (Translation.translate_term ctx) cond in
        let res = check c in
        Logger.debug "%s --> %s\n" (SMT.show c) (ThreeValuedLogic.show res);

        let continue_t () =
          Backend.push c;
          let res = unfold_predicate (n - 1) ctx id_map t in
          Backend.pop 1;
          res
        in

        let continue_e () =
          Backend.push (SMT.Boolean.mk_not c);
          let res = unfold_predicate (n - 1) ctx id_map e in
          Backend.pop 1;
          res
        in
        begin match res with
          | True -> continue_t ()
          | False -> continue_e ()
          | Unknown -> SL.mk_ite cond (continue_t ()) (continue_e ())
        end
(*
        if to_bool c_implied then (unfold_predicate (n - 1) ctx id_map t)
          else (
            Backend.push c;
            let res = unfold_predicate (n - 1) ctx id_map t in
            Backend.pop 1;
            SL.mk_ite cond res (unfold_predicate (n-1) ctx id_map e)
          )
        in



        let e' =
          if to_bool e_not_feasible then SL.ff
          else (
            Backend.push (SMT.Boolean.mk_not c);
            let res = unfold_predicate (n - 1) ctx id_map e in
            Backend.pop 1;
            res
          )
        in
        (*
        begin match t', e' with
          | None, None -> SL.ff
          | None, Some e -> SL.mk_star [cond; e]
          | Some t, None -> SL.mk_star [cond; t]
          | Some t, Some e -> SL.mk_ite cond t e
        *)
        SL.mk_ite cond t' e'*)
      | _ -> unfold_predicate (n - 1) ctx id_map def

  and unfold_predicate n ctx id_map phi =
    SL.map_view (function
      | Predicate (name, xs, _) when not @@ SID.is_builtin name ->
        let id = SID.get_definition name in
        unfold_step ctx n id id_map name xs
    ) phi


  let unfold input lhs rhs =
    let module C = Translation_context.Make(Encoding.Locations)(Encoding.HeapEncoding)in
    Profiler.add "Unfolding";
    let ctx = C.init input in
    let bound = LocationBounds.sum input.location_bounds in
    let id_map = SID.id_map () in
    let base = Backend.push lhs in
    let res = match Backend.check_sat SMT.Boolean.tt with
      | SMT_Unsat _ -> SL.tt
      | _ -> unfold_predicate bound ctx id_map rhs
    in
    Logger.debug "Performed %d SMT queries\n" !cnt;
    res


end
