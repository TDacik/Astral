(* Translation of SSL formulae to SMT.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open StackHeapModel

module Input = Context
module Context = Translation_context

module Printer = Printer.Make(struct let name = "Translation" end)

exception UnsupportedFragment of string

module Make (Encoding : Translation_sig.ENCODING) (Backend : Backend_sig.BACKEND) = struct

  open Encoding
  open Context
  open SMT

  module Footprints = Topped_set.Lift(SMT.Term.Collections.Set)

  (* ==== Helper functions for constructing common terms ==== *)

  let term_to_expr context t = match t with
    | SSL.Var x -> Locations.translate_var context.locs t
    | SSL.Pure psi -> psi
    | other -> failwith ("Cannot translate term " ^ SSL.show other)

  let var_to_expr context x = term_to_expr context (SSL.Var x)

  let formula_footprint ?(physically=true) context psi =
    let id = SSL.subformula_id ~physically context.phi psi in
    Format.asprintf "footprint%d" id

  let formula_footprint context phi =
    Set.mk_var (formula_footprint context phi) context.fp_sort

  (* ==== Recursive translation of SL formulae ==== *)

  let lift (semantics, axioms, footprints) = (semantics, axioms, Footprints.of_list footprints)

  let rec translate context phi domain = match phi with
    | SSL.Emp -> translate_emp context domain
    | SSL.PointsTo (x, y) -> translate_pointsto context domain x y
    | SSL.And (psi1, psi2) -> translate_and context domain psi1 psi2
    | SSL.Or (psi1, psi2) -> translate_or context domain psi1 psi2
    | SSL.Star psis -> translate_star context domain psis
    | SSL.Septraction (psi1, psi2) -> translate_septraction context domain phi psi1 psi2
    | SSL.Eq xs -> translate_eq context domain xs
    | SSL.Distinct xs -> translate_distinct context domain xs
    | SSL.LS (x, y) -> lift @@ LS_Encoding.translate context phi domain
    | SSL.DLS (x, y, f, l) -> lift @@ DLS_Encoding.translate context phi domain
    | SSL.NLS (x, y, z) -> lift @@ NLS_Encoding.translate context phi domain
    | SSL.Not (psi) -> translate_not context domain psi
    | SSL.GuardedNeg (psi1, psi2) -> translate_guarded_neg context domain psi1 psi2
    | SSL.Pure term -> translate_pure context domain term
    | SSL.Exists ([x], psi) -> translate_exists context domain x psi
    | SSL.Exists (x :: xs, psi) -> translate_exists context domain x (SSL.Exists (xs, psi))
    | _ -> failwith (Format.asprintf "%s" (SSL.show_with_sort phi))

  and translate_pure context domain term =
    let domain_def = Set.mk_eq_empty domain in
    let semantics = Boolean.mk_and [term; domain_def] in
    let axioms = Boolean.mk_true () in

    let fp = Set.mk_empty context.fp_sort in
    let footprints = Footprints.singleton fp in

    (semantics, axioms, footprints)

  and translate_emp context domain =
    let semantics = Set.mk_eq_empty domain in
    let axioms = Boolean.mk_true () in
    let fp = Set.mk_empty context.fp_sort in
    let footprints = Footprints.singleton fp in

    (semantics, axioms, footprints)

  and translate_pointsto context domain x y =
    let tx = term_to_expr context x in
    let domain_def = Set.mk_eq_singleton domain tx in
    let axioms = Boolean.mk_true () in
    let footprints = Footprints.singleton @@ Set.mk_singleton tx in
    let pointer = match y with
      | LS_t next ->
        SMT.mk_eq (term_to_expr context (Var next)) (HeapEncoding.mk_next context.heap tx)
      | DLS_t (next, prev) ->
        Boolean.mk_and
          [
            SMT.mk_eq (term_to_expr context (Var next)) (HeapEncoding.mk_next context.heap tx);
            SMT.mk_eq (term_to_expr context (Var prev)) (HeapEncoding.mk_prev context.heap tx);
          ]
      | NLS_t (top, next) ->
        Boolean.mk_and
          [
            SMT.mk_eq (term_to_expr context (Var next)) (HeapEncoding.mk_next context.heap tx);
            SMT.mk_eq (term_to_expr context (Var top )) (HeapEncoding.mk_top  context.heap tx);
          ]
    in
    let semantics = Boolean.mk_and [domain_def; pointer] in
    (semantics, axioms, footprints)

  and translate_eq context domain xs =
    let xs = List.map (term_to_expr context) xs in

    let domain_def = Set.mk_eq_empty domain in
    let equals = Boolean.mk_eq_list xs in
    let semantics = Boolean.mk_and [equals; domain_def] in
    let axioms = Boolean.mk_true () in

    let fp = Set.mk_empty context.fp_sort in
    let footprints = Footprints.singleton fp in

    (semantics, axioms, footprints)

  and translate_distinct context domain xs =
    let xs = List.map (term_to_expr context) xs in

    let domain_def = Set.mk_eq_empty domain in
    let distinct = Boolean.mk_distinct_list xs in
    let semantics = Boolean.mk_and [distinct; domain_def] in
    let axioms = Boolean.mk_true () in

    let fp = Set.mk_empty context.fp_sort in
    let footprints = Footprints.singleton fp in

    (semantics, axioms, footprints)

  and translate_not context domain phi =
    let context = {context with
      polarity = not context.polarity;
      can_skolemise = false
    }
    in

    let psi, axioms, footprints = translate context  phi domain in

    let semantics = Boolean.mk_not psi in
    let footprints = Footprints.top in
    (semantics, axioms, footprints)

  and translate_and context domain psi1 psi2 =
    let phi1, axioms1, footprints1 = translate context psi1 domain in
    let phi2, axioms2, footprints2 = translate context psi2 domain in

    let semantics = Boolean.mk_and [phi1; phi2] in
    let axioms = Boolean.mk_and [axioms1; axioms2] in

    let footprints =
      if not @@ Footprints.is_concrete footprints1 then footprints2
      else if not @@ Footprints.is_concrete footprints2 then footprints1
      else
        let size1 = Footprints.cardinal footprints1 in
        let size2 = Footprints.cardinal footprints2 in
        if size1 >= size2 then footprints2 else footprints1
    in

    (semantics, axioms, footprints)

  and translate_or context domain psi1 psi2 =
    let phi1, axioms1, footprints1 = translate context psi1 domain in
    let phi2, axioms2, footprints2 = translate context psi2 domain in

    let semantics = Boolean.mk_or [phi1; phi2] in
    let axioms = Boolean.mk_and [axioms1; axioms2] in
    let footprints = Footprints.union footprints1  footprints2 in
    (semantics, axioms, footprints)

  (** Translation of separating conjunction using skolemisation. *)
  and translate_star_skolemised context domain psis =
    let fps = List.map (formula_footprint context) psis in
    let semantics, axioms, footprints =
      List.map2 (translate context) psis fps
      |> List_utils.split3
    in

    let disjoint = Set.mk_disjoint_list fps in
    let fp_union = Set.mk_union fps context.fp_sort in
    let domain_def = Set.mk_eq domain fp_union in

    let axioms = Boolean.mk_and axioms in

    (* If star can be scolemised, footprints will never be used above it. *)
    let footprints = Footprints.top in

    let str_disjoint =
      (* TODO: Remove dependency on Options *)
      if List.for_all SSL.is_positive psis || not @@ Options_base.strong_separation ()
      then Boolean.mk_true ()
      else HeapEncoding.mk_strongly_disjoint context.heap context.smt_vars fps
    in
    (Boolean.mk_and (semantics @ [disjoint; domain_def; str_disjoint]), axioms, footprints)

  (** Translation of separating conjunction using second-order quantifiers. *)
  and translate_star_quantified context domain psis =
    (* Quantifier binders *)
    let fps =
      List.mapi (fun i _ -> Format.asprintf "FP%d" i) psis
      |> List.map (fun v -> Set.mk_fresh_var v context.fp_sort)
    in

    let semantics, axioms, footprints =
      List.map2 (translate context) psis fps
      |> List_utils.split3
    in

    (* Create lists of possible footprints. *)
    let fp_worklist = Footprints.apply_partial_variadic_op
      (fun fps ->
        if SMT.Set.may_disjoint fps
        then Some (Set.mk_union fps context.fp_sort)
        else None
      ) footprints
    in

    let axioms = Boolean.mk_and axioms in

    (* Unique footprints *)
    if List.for_all SSL.has_unique_footprint psis then begin
      assert (List.for_all (fun fp -> Footprints.cardinal fp <= 1) footprints);
      if List.exists Footprints.is_empty footprints then
        (Boolean.mk_false (), Boolean.mk_true (), Footprints.empty)
      else
      let fp_terms = List.map Footprints.choose footprints in
      let semantics = List_utils.map3 SMT.substitute semantics fps fp_terms in
      let disjoint = Set.mk_disjoint_list fp_terms in
      let fp_union = Set.mk_union fp_terms context.fp_sort in
      let domain_def = Set.mk_eq domain fp_union in
      let semantics = Boolean.mk_and (disjoint :: domain_def :: semantics) in
      (semantics, axioms, fp_worklist)
    end

    (* Not unique footprints --> quantifiers *)
    else
      let str_disjoint =
        (* TODO: Remove dependency on Options *)
        if List.for_all SSL.is_positive psis || not @@ Options_base.strong_separation ()
        then Boolean.mk_true ()
        else HeapEncoding.mk_strongly_disjoint context.heap context.smt_vars fps
      in
      let disjoint = Set.mk_disjoint_list fps in
      let fp_union = Set.mk_union fps context.fp_sort in
      let domain_def = Set.mk_eq domain fp_union in

      let ranges =
        try Some (List.map Footprints.elements footprints)
        with Footprints.TopError -> None
      in

      let fp_worklist, ranges =
        try
          if Option.is_some @@ Options_base.max_footprints () &&
            Footprints.cardinal fp_worklist > Option.get @@ Options_base.max_footprints () then
            Footprints.top, None
          else fp_worklist, ranges
        with _ -> Footprints.top, None
      in

      let semantics =
        Boolean.mk_and (disjoint :: domain_def :: str_disjoint :: semantics)
        |> Quantifier.mk_exists2 fps ~ranges
      in
      (semantics, axioms, fp_worklist)

  (** Generic translation of separating conjunction. *)
  and translate_star context domain psis =
    let context = {context with under_star = not context.can_skolemise} in
    if context.can_skolemise
    then translate_star_skolemised context domain psis
    else translate_star_quantified context domain psis

  and translate_guarded_neg context domain psi1 psi2 =
    let context' = {context with
      polarity = not context.polarity;
      can_skolemise = false
    }
    in
    let phi1, axioms1, footprints1 = translate context psi1 domain in
    let phi2, axioms2, footprints2 = translate context' psi2 domain in

    let phi2_neg = Boolean.mk_not phi2 in
    let semantics = Boolean.mk_and [phi1; phi2_neg] in
    let axioms = Boolean.mk_and [axioms1; axioms2] in
    (semantics, axioms, footprints1)

  (* TODO: unique shapes *)
  and translate_septraction context domain phi psi1 psi2 =
    let witness_heap_id = string_of_int @@ SSL.subformula_id context.phi phi in
    let witness_heap = HeapEncoding.mk ~suffix:witness_heap_id context.phi context.locs in

    if context.can_skolemise
    then translate_septraction_skolemised context domain witness_heap phi psi1 psi2
    else raise @@ UnsupportedFragment "magic wand/negative septraction"

  and translate_septraction_skolemised context domain witness_heap phi psi1 psi2 =
    let fp1 = formula_footprint context psi1 in
    let fp2 = formula_footprint context psi2 in

    (* Translation using witness heap *)
    let phi1, axioms1, footprints1 = translate {context with heap = witness_heap} psi1 fp1 in
    let phi2, axioms2, footprints2 = translate {context with heap = witness_heap} psi2 fp2 in

    let domain_def = Set.mk_eq domain (Set.mk_diff fp2 fp1) in
    let subset = Set.mk_subset fp1 fp2 in
    let heap_eq = HeapEncoding.mk_eq_on_domain context.heap witness_heap domain in
    let semantics = Boolean.mk_and [phi1; phi2; subset; heap_eq; domain_def] in

    let axioms = Boolean.mk_and [axioms1; axioms2] in

    (* If septraction can be scolemised, footprints will never be used above it. *)
    let footprints = Footprints.top in

    (semantics, axioms, footprints)

  (*
  (** Translation of septraction using Skolemization *)
  and translate_septraction_skolemised context domain h1 phi psi1 psi2 =
    (* Fresh footprint symbols *)
    let fp1 = formula_footprint context psi1 in
    let fp2 = formula_footprint context psi2 in

    let phi1, axioms1, footprints1 = translate {context with heap = h1} psi1 fp1 in
    let phi2, axioms2, footprints2 = translate {context with heap = h1} psi2 fp2 in

    let eq_fp = heaps_equal_on_footprint context context.heap h1 domain in
    let domain_def = Set.mk_eq domain (Set.mk_diff fp2 fp1) in

    let subset = Set.mk_subset fp1 fp2 in
    let axioms = Boolean.mk_and [axioms1; axioms2] in
    let semantics = Boolean.mk_and [phi1; phi2; subset; eq_fp; domain_def] in

    (* If septraction can be scolemised, footprint terms will never be used above it. *)
    let footprints = Footprints.top in

    (* Classical semantics *)
    if context.can_skolemise then
      (semantics, axioms, footprints)

    (* TODO: Negative septraction in WSL
    else if SSL.has_unique_shape psi1 && not @@ Options.strong_separation () then
      (semantics, axioms, footprints)

    (* TODO: Negative septraction in SSL *)
    else if SSL.has_unique_shape psi1 then
      let axiom, strongly_disjoint = mk_strongly_disjoint context fp1 domain in
      let semantics = Boolean.mk_and [semantics; strongly_disjoint] in
      let axioms = Boolean.mk_and [axioms; axiom] in
      (semantics, axioms, footprints)
    *)
    else raise UnsupportedFragment

  (** Translation of septraction without using Skolemization
  and translate_septraction_quantified context domain h1 phi psi1 psi2 =
    let fp1 = formula_footprint context psi1 in
    let fp2 = formula_footprint context psi2 in

    let semantics1, axioms1, footprints1 = translate {context with heap = h1} psi1 fp1 in
    let semantics2, axioms2, footprints2 = translate {context with heap = h1} psi2 fp2 in

    (* Currently unsupported fragment *)
    if Footprints.cardinal footprints1 != 1 then raise UnsupportedFragment;

    (* Translation based on the unique footprint *)
    let fp_term1 = Footprints.choose footprints1 in
    let semantics1 = SMT.substitute semantics1 fp1 fp_term1 in
    let disjoint = Set.mk_disjoint fp_term1 domain in
    let domain_def = Set.mk_eq fp2 (Set.mk_union [fp_term1; domain] context.fp_sort) in
    let eq_fp = heaps_equal_on_footprint context context.heap h1 (Set.mk_diff domain fp_term1) in
    let semantics = Boolean.mk_and [semantics2; disjoint; eq_fp; domain_def] in
    let axioms = Boolean.mk_and [axioms1; axioms2; semantics1] in

    let footprints =
      Footprints.apply_binop (fun x y -> Some (Set.mk_diff x y)) footprints2 footprints1
    in
    (semantics, axioms, footprints)
  *)
  *)
  (*= match SSL.get_sort x with
    | Sort.Bool -> translate_exists_bool context domain x psi
    | Sort.Loc -> translate_exists_loc context domain x psi

  and translate_exists_bool context domain x psi =
    let x = term_to_expr context x in
    let semantics, axioms, footprints = translate context psi domain in

    let semantics =
      [Boolean.mk_true (); Boolean.mk_false ()]
      |> List.map (SMT.substitute semantics x)
      |> Boolean.mk_or
    in

    let axioms =
      [Boolean.mk_true (); Boolean.mk_false ()]
      |> List.map (SMT.substitute axioms x)
      |> Boolean.mk_and
    in

    let footprints = Footprints.top in
    (semantics, axioms, footprints)

  and translate_exists_loc context domain x psi = *)

  and translate_exists context domain x psi =
    let x = term_to_expr context x in
    let semantics, axioms, footprints = translate context psi domain in

    let semantics = Quantifier.mk_exists [x] semantics in
    (*
      context.locs
      |> List.map (SMT.substitute semantics x)
      |> Boolean.mk_or
    in
    *)

    let footprints = Footprints.top
    (*
      context.locs
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

(*
let generate_heap_axioms context heap locs =
  List.map
    (fun x ->
      let hx = Array.mk_select heap x in
      Locations.var_axiom locs hx
    ) (Locations.get_constants context.locs)
  |> Boolean.mk_and
*)

let translate_phi (context : Context.t) ssl_phi =
  let footprint = formula_footprint context ssl_phi in
  let phi, axioms, _ = translate context ssl_phi context.global_footprint in
  let nil = SMT.Variable.mk "nil" context.loc_sort in
  let nil_not_in_fp = Boolean.mk_not (Set.mk_mem nil context.global_footprint) in

  (** Introduction of `next` array. May be ignored for positive formulae. *)
  let next_intro = SMT.mk_eq nil @@ HeapEncoding.mk_next context.heap nil in

  let location_axioms = Locations.axioms context.locs context.phi in
  let heap_axioms = HeapEncoding.axioms context.heap in
  let location_lemmas = Locations.lemmas context.locs in

  Boolean.mk_and
    [
      phi; axioms; nil_not_in_fp; location_lemmas;
      next_intro;
      location_axioms; heap_axioms
    ]

  (* ==== Translation of SMT model to stack-heap model ==== *)

  let nil_interp context model =
    let e = SMT.Variable.mk (SSL.Variable.show SSL.Variable.nil) context.loc_sort in
    try Model.eval model e
    with _ -> failwith "No interpretation of nil"

  let translate_stack context model =
    List.fold_left
      (fun stack var ->
        let var_expr = SMT.Variable.mk (SSL.Variable.show var) context.loc_sort in
        let loc =
          try Model.eval model var_expr
          with Not_found -> nil_interp context model
        in
        Stack.add var (Locations.inverse_translate context.locs model loc) stack
      ) Stack.empty (SSL.Variable.nil :: context.vars)

  (** TODO: translate ALL footprint symbols introduced during translation. *)
  let translate_footprints context model =
    List.fold_left
      (fun acc (psi, fp_expr) ->
        let set = Set.get_elems @@ Model.eval model fp_expr in
        let fp = Footprint.of_list @@ List.map (Locations.inverse_translate context.locs model) set in
        SSL.Map.add psi fp acc
      ) SSL.Map.empty [(context.phi, context.global_footprint)]

  (* TODO: translation of witness heaps *)
  let translate_witness_heaps context model = SSL.Map.empty

  let translate_model context model =
    let s = translate_stack context model in
    let h = HeapEncoding.inverse_translate context.heap model context.global_footprint in
    let footprints = translate_footprints context model in
    let heaps = translate_witness_heaps context model in
    StackHeapModel.init ~heaps s h


  let log (input : Input.t) context =
    Printer.debug "Translating:
     - Location encoding: %s
     - Heap encoding: %s
     - Set encoding: %s
     - Quantifier encoding: %s
     - Predicate encodings:
        - ls: %s
        - dls: %s
        - nls: %s
     - Backend: %s\n"
     Locations.name
     HeapEncoding.name
     SetEncoding.name
     QuantifierEncoding.name
     LS_Encoding.name
     DLS_Encoding.name
     NLS_Encoding.name
     Backend.name

  (* ==== Solver ==== *)
  let solve input =
    let context = Context.init input in
    log input context;

    (* Translation *)
    let translated1 = translate_phi context input.phi in
    Debug.translated ~suffix:"1" translated1;

    (* Set rewritting *)
    let translated2 = SetEncoding.rewrite translated1 in
    Debug.translated ~suffix:"2_set_encoding" translated2;

    (* Quantifier rewritting *)
    let translated3 = QuantifierEncoding.rewrite context.locs translated2 in
    Debug.translated ~suffix:"3_qf_rewriting" translated3;

    (* Backend preprocessor *)
    let translated = Backend_preprocessor.apply translated3 in
    Debug.translated ~suffix:"4_backend_preprocessing" translated;

    let size = SMT.Term.size translated in
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

    Printer.debug "Running backend SMT solver\n";
    Profiler.add "Translation";

    (* Solve *)
    match Backend.solve input translated produce_models user_options with
    | SMT_Sat None -> Input.set_result `Sat input
    | SMT_Sat (Some (smt_model, backend_model)) ->
      let _ = Debug.backend_model (Backend.show_model backend_model) in
      let smt_model = SetEncoding.rewrite_back translated1 smt_model in
      let _ = Debug.smt_model smt_model in
      let sh = translate_model context smt_model in
      let _ = Debug.model sh in
      Input.set_result `Sat ~model:(Some sh) input

    (* TODO: unsat cores *)
    | SMT_Unsat unsat_core -> Input.set_result `Unsat ~unsat_core:(Some []) input

    (* TODO: remove duplicit reason *)
    | SMT_Unknown reason -> Input.set_result (`Unknown reason) ~reason:(Some reason) input

  let solve input =
    try solve input
    with UnsupportedFragment str ->
      let reason = "unsupported SL fragment: " ^ str in
      Input.set_result (`Unknown reason) ~reason:(Some reason) input
end
