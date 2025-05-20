
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
    | SL.PointsTo (x, _, _) -> [x]
    | SL.Predicate (name, params, _) ->
      let abstraction = SID.abstraction name in
      [PredicateAbstraction.get_root abstraction ~params]

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

  let rec unfold_step ctx n ~allocated ~existential id id_map name xs =
    Logger.debug "Remaining: %d\n" n;
    if n = 0 then unfold_finite id xs
    else
      let def = instantiate ~refresh:true id xs in
      match SL.view def with
      (* TODO: existential prefix for ite*)
      | Ite (cond, t, e) ->
        SL.print cond;
        let c = SL.translate_pure_with_heap_term (Translation.translate_term ctx) cond in
        (*let c = SMT.Quantifier.mk_exists existential c in *)
        let res = check c in
        Logger.debug "%s --> %s\n" (SMT.show c) (ThreeValuedLogic.show res);

        let continue_t () =
          Backend.push c;
          (*Backend.push @@ pure_abstraction ctx t;*)
          let existential = existential @ (List.map (Translation.translate_var ctx) (SL.bound_vars t)) in
          let res = unfold_predicate ~existential (n - 1) ctx id_map t in
          Backend.pop 1;
          res
        in

        let continue_e () =
          Backend.push (SMT.Boolean.mk_not c);
          (*Backend.push @@ pure_abstraction ctx e;*)
          let existential = existential @ (List.map (Translation.translate_var ctx) (SL.bound_vars e)) in
          let res = unfold_predicate ~existential (n - 1) ctx id_map e in
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
          SMT.print ~prefix:"Going to check" c;
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
              SL.mk_or [acc; (Backend.push c; let res = unfold_predicate (n-1) ~allocated ctx id_map case in Backend.pop 1; res)]
          in
          (if Options_base.fp_construction ()
           then
             cache := SL.Map.add res (Footprints.mk_footprint ctx ~allocated !lhs_t id xs) !cache
           else ()
          );
          res
          ) SL.ff cases

      | _ -> failwith @@ SL.show def (* unfold_predicate (n - 1) ctx id_map def*)

  and unfold_predicate n ?(allocated=[]) ?(existential=[]) ctx id_map phi =
    SL.map_view (function
      | Predicate (name, xs, _) when not @@ SID.is_builtin name ->
        let id = SID.get_definition name in
        `Modify (unfold_step ctx n ~allocated ~existential id id_map name xs)
      | _ -> `Skip
    ) phi


  let unfold input lhs rhs =
    let module C = Translation_context.Make(Encoding.Locations)(Encoding.HeapEncoding) in
    Profiler.add "Unfolding";
    cache := SL.Map.empty;
    let ctx = C.init input in
    let bound = LocationBounds.sum input.location_bounds - 1 in (* For nil *)
    let id_map = SID.id_map () in
    lhs_t := lhs;
    (** TODO: add axioms to lhs *)
    let base = Backend.push lhs in
    let res = match Backend.check_sat lhs with
      | SMT_Unsat _ ->
        Logger.debug "LHS is UNSAT\n";
        SL.tt
      | _ -> unfold_predicate bound ctx id_map rhs
    in
    Backend.pop 1;
    Logger.debug "Performed %d SMT queries\n" !cnt;
    res, !cache


end
