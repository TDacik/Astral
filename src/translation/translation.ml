(* Translation of SL formulae to SMT.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open MemoryModel
open StackHeapModel

module Input = Context
module Context = Translation_context

module Logger = Logger.Make(struct let name = "Translation" let level = 1 end)

module Make (Encoding : Translation_sig.ENCODING) (Backend : Backend_sig.BACKEND) = struct

  open SMT
  open Encoding

  module Footprints = Topped_set.Lift(SMT.Set)
  module PredicateTranslation = SID.Translation(Encoding)

  (** Definition of context type *)
  open Encoding_context_sig

  let debug_info ctx =
    Logger.debug "\n  Locations: %s\n%s"
      (Locations.show ctx.locs)
      (HeapEncoding.show ctx.heap)

  (* ==== Helper functions for constructing common terms ==== *)
  (* TODO: we currently assume that there are no heap-terms under begin/end *)

  let translate_var ctx var = Locations.translate_var ctx.locs var

  let translate_heap_term ctx (field : MemoryModel.Field.t) x = HeapEncoding.mk_succ ctx.heap field x

  let translate_block_begin ctx x = SMT.Array.mk_select ctx.block_begin x

  let translate_block_end ctx x = SMT.Array.mk_select ctx.block_end x

  (** We can use Obj.magic here because we know that SL.t and SMT.t are
      internaly the same type. TODO: but is weird... *)
  let rec translate_term ctx t : SMT.t =
    Obj.magic (match SL.Term.view t with
      | SL.Term.Var x -> SMT.of_var @@ Locations.translate_var ctx.locs x
      | SL.Term.HeapTerm (f, x) -> translate_heap_term ctx f (translate_term ctx x)
      | SL.Term.SmtTerm _ -> Locations.translate_term ctx.locs t
      | SL.Term.BlockBegin x -> translate_block_begin ctx (translate_term ctx x)
      | SL.Term.BlockEnd x -> translate_block_end ctx (translate_term ctx x)
    )

  (** Translate "almost-pure" formula by replacing heap terms by select from
      corresponding arrays. *)
  let translate_pure_with_heap_terms ctx phi =
    SMT.of_base_logic @@ BaseLogic.map (function
      | BaseLogic.Application (HeapTerm f, [x]) ->
        SMT.to_base_logic @@ translate_heap_term ctx f (SMT.of_base_logic x)
      | BaseLogic.Variable var -> BaseLogic.Variable (Obj.magic @@ translate_var ctx (Obj.magic var))
      | x -> x
    ) (SL.to_base_logic phi)



  let id = ref 0

  (** Currently footprint ID does not match ID in printed AST *)
  let formula_footprint ctx phi =
    id := !id + 1;
    let name = Format.asprintf "footprint%d" !id (*SL.subformula_id ctx.phi phi*) in
    Sets.mk_var name ctx.fp_sort

  (* ==== Recursive translation of SL formulae ==== *)

  let lift (semantics, axioms, footprints) = (semantics, axioms, Footprints.of_list footprints)

  let rec translate ctx domain phi = match SL.view phi with
    | SL.Emp -> translate_emp ctx domain
    | SL.True -> translate_true ctx domain
    | SL.False -> translate_false ctx domain
    | SL.PointsTo (x, struct_def, ys) -> translate_pointsto ctx domain x struct_def ys
    | SL.And psis -> translate_and ctx domain psis
    | SL.Or psis -> translate_or ctx domain psis
    | SL.Star psis -> translate_star ctx domain psis
    | SL.Septraction (psi1, psi2) -> translate_septraction ctx domain phi psi1 psi2
    | SL.Eq xs -> translate_eq ctx domain xs
    | SL.Distinct xs -> translate_distinct ctx domain xs
    | SL.Predicate (id, xs, defs) -> translate_predicate ctx domain id xs defs
    | SL.Not (psi) -> translate_not ctx domain psi
    | SL.GuardedNeg (psi1, psi2) -> translate_guarded_neg ctx domain psi1 psi2
    | SL.Ite (cond, then_, else_) -> translate_ite ctx domain cond then_ else_
    | SL.Pure term -> translate_pure ctx domain term
    | SL.Exists ([x], psi) -> translate_exists ctx domain x psi
    | SL.Exists (x :: xs, psi) -> translate_exists ctx domain x (SL.mk_exists xs psi)
    | _ -> Utils.internal_error
            (Format.asprintf "Translation: unknown term %s" (SL.show_with_sort phi))

  and translate_true ctx domain =
    (Boolean.tt, Boolean.tt, Footprints.singleton @@ Sets.mk_empty ctx.fp_sort)

  and translate_false ctx domain =
    (Boolean.ff, Boolean.tt, Footprints.singleton @@ Sets.mk_empty ctx.fp_sort)

  and translate_pure ctx domain term =
    let domain_def = Sets.mk_eq_empty domain in
    let semantics = Boolean.mk_and [term; domain_def] in
    let axioms = Boolean.tt in

    let fp = Sets.mk_empty ctx.fp_sort in
    let footprints = Footprints.singleton fp in

    (semantics, axioms, footprints)

  and translate_emp ctx domain =
    let semantics = Sets.mk_eq_empty domain in
    let axioms = Boolean.tt in
    let fp = Sets.mk_empty ctx.fp_sort in
    let footprints = Footprints.singleton fp in

    (semantics, axioms, footprints)

  and translate_pointsto ctx domain x struct_def ys =
    let x = translate_term ctx x in
    let ys = List.map (translate_term ctx) ys in
    let axioms = Boolean.tt in

    let domain_def = Sets.mk_eq_singleton domain x in
    let footprints = Footprints.singleton @@ Sets.mk_singleton x in
    let fields = StructDef.get_fields struct_def in
    let pointer =
      List.map2 (fun field y ->
        SMT.mk_eq [HeapEncoding.mk_succ ctx.heap field x; y]
      ) fields ys
      |> Boolean.mk_and
    in
    let semantics = Boolean.mk_and [domain_def; pointer] in
    (semantics, axioms, footprints)

  and translate_predicate ctx domain id xs defs =
    let sxs = List.map (translate_term ctx) xs in
    let semantics, axioms, fps = PredicateTranslation.translate id ctx (xs, defs) domain sxs in
    let footprints = Footprints.of_list fps in
    (semantics, axioms, footprints)

  and translate_eq ctx domain xs =
    let xs = List.map (translate_term ctx) xs in

    let domain_def = Sets.mk_eq_empty domain in
    let equals = Boolean.mk_eq xs in
    let semantics = Boolean.mk_and [equals; domain_def] in
    let axioms = Boolean.tt in

    let fp = Sets.mk_empty ctx.fp_sort in
    let footprints = Footprints.singleton fp in

    (semantics, axioms, footprints)

  and translate_distinct ctx domain xs =
    let xs = List.map (translate_term ctx) xs in

    let domain_def = Sets.mk_eq_empty domain in
    let distinct = Boolean.mk_distinct xs in
    let semantics = Boolean.mk_and [distinct; domain_def] in
    let axioms = Boolean.tt in

    let fp = Sets.mk_empty ctx.fp_sort in
    let footprints = Footprints.singleton fp in

    (semantics, axioms, footprints)

  and translate_not ctx domain phi =
    let ctx = {ctx with
      polarity = not ctx.polarity;
      can_skolemise = false
    }
    in

    let psi, axioms, footprints = translate ctx domain phi in

    let semantics = Boolean.mk_not psi in
    let footprints = Footprints.top in
    (semantics, axioms, footprints)

  and translate_and ctx domain psis =
    let semantics, axioms, footprints =
      List_utils.split3 @@ List.map (translate ctx domain) psis
    in

    let semantics = Boolean.mk_and semantics in
    let axioms = Boolean.mk_and axioms in

    let footprints =
      let concrete = BatList.filter Footprints.is_concrete footprints in
      let sizes = BatList.map Footprints.cardinal concrete in
      match concrete with
        | [] -> List.hd footprints
        | xs ->
          let min = BatList.min sizes in
          List.find (fun fp -> Footprints.cardinal fp == min) concrete
    in

    (semantics, axioms, footprints)

  and translate_or ctx domain psis =
    let semantics, axioms, footprints =
      List_utils.split3 @@ List.map (translate ctx domain) psis
    in

    let semantics = Boolean.mk_or semantics in
    let axioms = Boolean.mk_and axioms in
    let footprints = List.fold_left Footprints.union Footprints.empty footprints in
    (semantics, axioms, footprints)

  and translate_ite ctx domain cond then_ else_ =
    let semantics1, axioms1, footprints1 = translate ctx domain then_ in
    let semantics2, axioms2, footprints2 = translate ctx domain else_ in
    let cond = translate_pure_with_heap_terms ctx cond in

    let semantics = Boolean.mk_ite cond semantics1 semantics2 in
    let axioms = Boolean.mk_and [axioms1; axioms2] in
    let footprints = Footprints.map2 (Boolean.mk_ite cond) footprints1 footprints2 in
    (semantics, axioms, footprints)


  (** Translation of separating conjunction using skolemisation. *)
  and translate_star_skolemised ctx domain psis =
    let fps = List.map (formula_footprint ctx) psis in
    let semantics, axioms, footprints =
      List.map2 (translate ctx) fps psis
      |> List_utils.split3
    in

    let disjoint = Sets.mk_disjoint fps in
    let fp_union = Sets.mk_union ctx.fp_sort fps in
    let domain_def = Sets.mk_eq [domain; fp_union] in

    let axioms = Boolean.mk_and axioms in

    (* If star can be scolemised, footprints will never be used above it. *)
    let footprints = Footprints.top in

    let str_disjoint =
      (* TODO: Remove dependency on Options *)
      if List.for_all SL.is_positive psis || not @@ Options_base.strong_separation ()
      then Boolean.tt
      else
        let vars = List.map SMT.of_var ctx.smt_vars in
        HeapEncoding.mk_strongly_disjoint ctx.heap vars fps
    in
    (Boolean.mk_and (semantics @ [disjoint; domain_def; str_disjoint]), axioms, footprints)

  (** Translation of separating conjunction using second-order quantifiers. *)
  and translate_star_quantified ctx domain psis =
    (* Quantifier binders *)
    let fp_vars =
      List.mapi (fun i _ -> Format.asprintf "FP%d" i) psis
      |> List.map (fun v -> Variable.mk_fresh v ctx.fp_sort)
    in
    let fps = List.map SMT.of_var fp_vars in

    let semantics, axioms, footprints =
      List.map2 (translate ctx) fps psis
      |> List_utils.split3
    in

    (* Create lists of possible footprints. *)
    let fp_worklist = Footprints.apply_partial_variadic_op
      (fun fps ->
        if SMT.Sets.may_disjoint fps
        then Some (Sets.mk_union ctx.fp_sort fps)
        else None
      ) footprints
    in

    let axioms = Boolean.mk_and axioms in

    (* Unique footprints *)
    if List.for_all SLID.has_unique_footprint psis then begin
      assert (List.for_all (fun fp -> Footprints.cardinal fp <= 1) footprints);
      if List.exists Footprints.is_empty footprints then
        (Boolean.ff, Boolean.tt, Footprints.empty)
      else
      let fp_terms = List.map Footprints.choose footprints in
      let semantics =
        List_utils.map3
          (fun phi fp fp_term -> SMT.substitute phi ~var:fp ~by:fp_term) semantics fp_vars fp_terms
      in
      let disjoint = Sets.mk_disjoint fp_terms in
      let fp_union = Sets.mk_union ctx.fp_sort fp_terms in
      let domain_def = Sets.mk_eq [domain; fp_union] in
      let semantics = Boolean.mk_and (disjoint :: domain_def :: semantics) in
      (semantics, axioms, fp_worklist)
    end

    (* Not unique footprints --> quantifiers *)
    else
      let str_disjoint =
        (* TODO: Remove dependency on Options *)
        if List.for_all SL.is_positive psis || not @@ Options_base.strong_separation ()
        then Boolean.tt
        else
          let vars = List.map SMT.of_var ctx.smt_vars in
          HeapEncoding.mk_strongly_disjoint ctx.heap vars fps
      in
      let disjoint = Sets.mk_disjoint fps in
      let fp_union = Sets.mk_union ctx.fp_sort fps in
      let domain_def = Sets.mk_eq [domain; fp_union] in

      (* TODO: can we use lazy better? *)
      let ranges = List.map (fun x -> lazy (Footprints.elements x)) footprints in

      let fp_worklist, ranges =
        try
          if Option.is_some @@ Options_base.max_footprints () &&
            Footprints.cardinal fp_worklist > Option.get @@ Options_base.max_footprints () then
            Footprints.top, None
          else fp_worklist, Some ranges
        with _ -> Footprints.top, None
      in

      let body = Boolean.mk_and (disjoint :: domain_def :: str_disjoint :: semantics) in
      let semantics = match ranges with
        | None -> Quantifier.mk_exists2 fp_vars body
        | Some r -> Quantifier.mk_exists2 ~ranges:r fp_vars body
      in
      (semantics, axioms, fp_worklist)

  (** Generic translation of separating conjunction. *)
  and translate_star ctx domain psis =
    let ctx = {ctx with under_star = not ctx.can_skolemise} in
    if ctx.can_skolemise
    then translate_star_skolemised ctx domain psis
    else translate_star_quantified ctx domain psis

  and translate_guarded_neg ctx domain psi1 psi2 =
    let ctx' = {ctx with
      polarity = not ctx.polarity;
      can_skolemise = false
    }
    in
    let phi1, axioms1, footprints1 = translate ctx domain psi1 in
    let phi2, axioms2, footprints2 = translate ctx' domain psi2 in

    let phi2_neg = Boolean.mk_not phi2 in
    let semantics = Boolean.mk_and [phi1; phi2_neg] in
    let axioms = Boolean.mk_and [axioms1; axioms2] in
    (semantics, axioms, footprints1)

  (* TODO: unique shapes *)
  and translate_septraction ctx domain phi psi1 psi2 =
    let id = string_of_int !id in
    let witness_heap = HeapEncoding.mk ctx.phi ~suffix:id ctx.heap_sort ctx.locs in

    if ctx.can_skolemise
    then translate_septraction_skolemised ctx domain witness_heap phi psi1 psi2
    else raise @@ SolverUtils.UnsupportedFragment "magic wand/negative septraction"

  and translate_septraction_skolemised ctx domain witness_heap phi psi1 psi2 =
    let fp1 = formula_footprint ctx psi1 in
    let fp2 = formula_footprint ctx psi2 in

    (* Translation using witness heap *)
    let phi1, axioms1, footprints1 = translate {ctx with heap = witness_heap} fp1 psi1 in
    let phi2, axioms2, footprints2 = translate {ctx with heap = witness_heap} fp2 psi2 in

    let domain_def = Sets.mk_eq [domain; Sets.mk_diff fp2 fp1] in
    let subset = Sets.mk_subset fp1 fp2 in
    let heap_eq = HeapEncoding.mk_eq_on_domain ctx.heap witness_heap domain in
    let semantics = Boolean.mk_and [phi1; phi2; subset; heap_eq; domain_def] in

    let axioms = Boolean.mk_and [axioms1; axioms2] in

    (* If septraction can be scolemised, footprints will never be used above it. *)
    let footprints = Footprints.top in

    (semantics, axioms, footprints)

  (*
  (** Translation of septraction using Skolemization *)
  and translate_septraction_skolemised ctx domain h1 phi psi1 psi2 =
    (* Fresh footprint symbols *)
    let fp1 = formula_footprint ctx psi1 in
    let fp2 = formula_footprint ctx psi2 in

    let phi1, axioms1, footprints1 = translate {ctx with heap = h1} psi1 fp1 in
    let phi2, axioms2, footprints2 = translate {ctx with heap = h1} psi2 fp2 in

    let eq_fp = heaps_equal_on_footprint ctx ctx.heap h1 domain in
    let domain_def = Sets.mk_eq domain (Sets.mk_diff fp2 fp1) in

    let subset = Sets.mk_subset fp1 fp2 in
    let axioms = Boolean.mk_and [axioms1; axioms2] in
    let semantics = Boolean.mk_and [phi1; phi2; subset; eq_fp; domain_def] in

    (* If septraction can be scolemised, footprint terms will never be used above it. *)
    let footprints = Footprints.top in

    (* Classical semantics *)
    if ctx.can_skolemise then
      (semantics, axioms, footprints)

    (* TODO: Negative septraction in WSL
    else if SL.has_unique_shape psi1 && not @@ Options.strong_separation () then
      (semantics, axioms, footprints)

    (* TODO: Negative septraction in SL *)
    else if SL.has_unique_shape psi1 then
      let axiom, strongly_disjoint = mk_strongly_disjoint ctx fp1 domain in
      let semantics = Boolean.mk_and [semantics; strongly_disjoint] in
      let axioms = Boolean.mk_and [axioms; axiom] in
      (semantics, axioms, footprints)
    *)
    else raise UnsupportedFragment

  (** Translation of septraction without using Skolemization
  and translate_septraction_quantified ctx domain h1 phi psi1 psi2 =
    let fp1 = formula_footprint ctx psi1 in
    let fp2 = formula_footprint ctx psi2 in

    let semantics1, axioms1, footprints1 = translate {ctx with heap = h1} psi1 fp1 in
    let semantics2, axioms2, footprints2 = translate {ctx with heap = h1} psi2 fp2 in

    (* Currently unsupported fragment *)
    if Footprints.cardinal footprints1 != 1 then raise UnsupportedFragment;

    (* Translation based on the unique footprint *)
    let fp_term1 = Footprints.choose footprints1 in
    let semantics1 = SMT.substitute semantics1 fp1 fp_term1 in
    let disjoint = Sets.mk_disjoint fp_term1 domain in
    let domain_def = Sets.mk_eq fp2 (Sets.mk_union [fp_term1; domain] ctx.fp_sort) in
    let eq_fp = heaps_equal_on_footprint ctx ctx.heap h1 (Sets.mk_diff domain fp_term1) in
    let semantics = Boolean.mk_and [semantics2; disjoint; eq_fp; domain_def] in
    let axioms = Boolean.mk_and [axioms1; axioms2; semantics1] in

    let footprints =
      Footprints.apply_binop (fun x y -> Some (Sets.mk_diff x y)) footprints2 footprints1
    in
    (semantics, axioms, footprints)
  *)
  *)
  (*= match SL.get_sort x with
    | Sort.Bool -> translate_exists_bool ctx domain x psi
    | Sort.Loc -> translate_exists_loc ctx domain x psi

  and translate_exists_bool ctx domain x psi =
    let x = translate_term ctx x in
    let semantics, axioms, footprints = translate ctx psi domain in

    let semantics =
      [Boolean.tt; Boolean.ff]
      |> List.map (SMT.substitute semantics x)
      |> Boolean.mk_or
    in

    let axioms =
      [Boolean.tt; Boolean.ff]
      |> List.map (SMT.substitute axioms x)
      |> Boolean.mk_and
    in

    let footprints = Footprints.top in
    (semantics, axioms, footprints)

  and translate_exists_loc ctx domain x psi = *)

  and translate_exists ctx domain x psi =
    let x = Locations.translate_var ctx.locs x in
    let semantics, axioms, footprints = translate ctx domain psi in

    let semantics = Quantifier.mk_exists [x] semantics in
    (*
      ctx.locs
      |> List.map (SMT.substitute semantics x)
      |> Boolean.mk_or
    in
    *)

    let footprints = Footprints.top
    (*
      ctx.locs
      |> List.map
          (fun loc ->
            Footprints.map
              (fun fp ->
                SMT.substitute fp x loc
              ) footprints
          )
      |> List.fold_left Footprints.union Footprints.empty
    *)
    in

    (* TODO: check footprint *)
    (semantics, axioms, footprints)

(** Axioms for formulae containing begin/end terms. *)
let low_level_axioms ctx = (* TODO *)
  let width = Sort.get_width @@ List.hd @@ HeapSort.get_loc_sorts @@ ctx.heap_sort in
  Locations.mk_forall' ctx.locs 2 (fun [loc; loc'] ->
    let begin_loc = SMT.Array.mk_select ctx.block_begin loc in
    let end_loc = SMT.Array.mk_select ctx.block_end loc in
    let begin_loc' = SMT.Array.mk_select ctx.block_begin loc' in
    let end_loc' = SMT.Array.mk_select ctx.block_end loc' in
    let axiom1 =
      SMT.Boolean.mk_or [
        SMT.mk_eq [loc; begin_loc; end_loc];
        SMT.Boolean.mk_and [
          SMT.Bitvector.mk_lesser_eq begin_loc loc;
          SMT.Bitvector.mk_lesser loc end_loc;
        ]
      ]
    in
    (* Locations are non-overlapping *)
    let axiom2 =
      let overlap_case1 = SMT.Boolean.mk_and [
        SMT.Bitvector.mk_lesser begin_loc begin_loc';
        SMT.Bitvector.mk_lesser begin_loc' end_loc;
      ] in
      let overlap_case2 = SMT.Boolean.mk_and [
        SMT.Bitvector.mk_lesser begin_loc' begin_loc;
        SMT.Bitvector.mk_lesser begin_loc end_loc';
      ] in
      let pre = SMT.Boolean.mk_or [overlap_case1; overlap_case2] in
      let post = SMT.Boolean.mk_and [
        SMT.mk_eq [begin_loc; begin_loc'];
        SMT.mk_eq [end_loc; end_loc'];
      ] in
      SMT.Boolean.mk_implies pre post
    in
    SMT.Boolean.mk_and [axiom1; axiom2]
    )

let translate_phi (ctx : Context.t) ssl_phi =
  let footprint = formula_footprint ctx ssl_phi in
  let phi, axioms, _ = translate ctx ctx.global_footprint ssl_phi in
  let nil = SMT.mk_var "nil" ctx.loc_sort in
  let nil_not_in_fp = Boolean.mk_not (Sets.mk_mem nil ctx.global_footprint) in

  (**
      TODO: need to be reworked...
   Introduction of `next` array. May be ignored for positive formulae.
  let next = Field.next in
  let next_intro = SMT.mk_eq [nil; HeapEncoding.mk_succ ctx.heap next nil] in
  *)

  let location_axioms = Locations.axioms ctx.locs ctx.phi in
  let heap_axioms = HeapEncoding.axioms ctx.heap in
  let location_lemmas = Locations.lemmas ctx.locs in

  let low_level_axioms =
    if SL.is_low_level ssl_phi
    then low_level_axioms ctx
    else SMT.Boolean.tt
  in

  Boolean.mk_and
    [
      phi; axioms; nil_not_in_fp; location_lemmas;
      (*next_intro;*)
      low_level_axioms;
      location_axioms; heap_axioms
    ]

  (* ==== Translation of SMT model to stack-heap model ==== *)

  let nil_interp ctx model =
    let e = SMT.mk_var (SL.Variable.show SL.Variable.nil) ctx.loc_sort in
    try Model.eval model e
    with _ -> failwith "No interpretation of nil"

  let translate_stack ctx model =
    let stack = List.fold_left
      (fun stack term ->
        let const = match SL.Term.view term with
          | SL.Term.SmtTerm c -> Some (StackHeapModel.Location.mk_smt @@ Model.eval model c)
          | SL.Term.Var var when not @@ SL.Variable.is_loc var ->
            Some (StackHeapModel.Location.mk_smt
            @@ Model.eval model @@ Locations.mk_var ctx.locs (SL.Variable.show var))
          | SL.Term.Var var ->
            Some (Locations.inverse_translate ctx.locs model
            @@ Model.eval model @@ Locations.mk_var ctx.locs (SL.Variable.show var))
          | SL.Term.HeapTerm _ -> None

        in match const with
        | None -> stack
        | Some const -> Stack.M.add term const stack
      ) Stack.M.empty (SL.Term.nil :: ctx.location_terms)
    in
    Stack.mk model stack

  let translate_footprint ctx model fp =
    Model.eval model fp
    |> Constant.get_elems
    |> List.map (fun loc -> SMT.of_const loc, Locations.inverse_translate ctx.locs model loc)

  let translate_model ctx model =
    let domain = translate_footprint ctx model ctx.global_footprint in
    let s = translate_stack ctx model in
    let h = HeapEncoding.inverse_translate ctx.heap model domain in
    StackHeapModel.init s h


  let log (input : Input.t) ctx =
    Logger.debug "Translating:
     - Location encoding: %s
     - Heap encoding: %s
     - Set encoding: %s
     - Quantifier encoding: %s
     - Backend: %s\n"
     Locations.name
     HeapEncoding.name
     SetEncoding.name
     QuantifierEncoding.name
     Backend.name

  (* ==== Solver ==== *)
  let solve input =
    let ctx = Context.init input in
    debug_info ctx;
    log input ctx;

    (* Translation *)
    let translated1 = translate_phi ctx input.phi in
    Debug.translated ~suffix:"1" translated1;

    (* Set rewritting *)
    let translated2 = SetEncoding.rewrite translated1 in
    Debug.translated ~suffix:"2_set_encoding" translated2;

    (* Quantifier rewritting *)
    let translated3 = QuantifierEncoding.rewrite ctx.locs translated2 in
    Debug.translated ~suffix:"3_qf_rewriting" translated3;

    (* Backend preprocessor *)
    let translated = Backend_preprocessor.apply translated3 in
    Debug.translated ~suffix:"4_backend_preprocessing" translated;

    let size = SMT.size translated in
    let input = Input.set_size input size in

    (* TODO: Remove dependency on Options *)
    let produce_models = input.raw_input.produce_models || Options_base.produce_models () in
    let user_options = Options_base.backend_options () in

    Backend.init ();
    let backend_translated = Backend.translate translated in

    Debug.context input;
    Debug.backend_translated (Backend.show_formula backend_translated);
    Debug.backend_simplified (Backend.show_formula @@ Backend.simplify backend_translated);
    Debug.backend_input (Backend.to_smtlib translated produce_models user_options);

    Logger.debug "Running backend SMT solver\n";
    Profiler.add "Translation";

    (* Solve *)
    let result = Backend.solve ctx translated produce_models user_options in
    Profiler.add "SMT backend";

    match result with
    | SMT_Sat None -> Input.set_result `Sat input
    | SMT_Sat (Some (smt_model, backend_model)) ->
      let smt_model = SetEncoding.rewrite_back translated1 smt_model in
      let _ = Debug.smt_model smt_model in
      let sh = translate_model ctx smt_model in
      let _ = Debug.model sh in
      Input.set_result `Sat ~model:(Some sh) input

    (* TODO: unsat cores *)
    | SMT_Unsat unsat_core -> Input.set_result `Unsat ~unsat_core:(Some []) input

    (* TODO: remove duplicit reason *)
    | SMT_Unknown reason -> Input.set_result (`Unknown reason) ~reason:(Some reason) input

  let solve input =
    try solve input
    with SolverUtils.UnsupportedFragment str ->
      let reason = "unsupported SL fragment: " ^ str in
      Input.set_result (`Unknown reason) ~reason:(Some reason) input
end
