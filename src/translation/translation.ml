(* Translation of SSL formulae to SMT.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open StackHeapModel

module Input = Context
module Context = Translation_context

module Print = Printer.Make(struct let name = "Translation" end)

module Make (Encoding : Translation_sig.ENCODING) (Backend : Backend_sig.BACKEND) = struct

  open Encoding
  open SMT
  open Context

  module DLListEncoding = DLListEncoding.Make(Encoding.Locations)

  (** Initialization of Z3 context and translation context *)
  let init (info : Input.t) =
    let locs_card = info.location_bound + 1 in (* one for nil *)
    let locs = Locations.mk "Loc" locs_card in
    let locs_sort = Locations.get_sort locs in
    let loc_consts = Locations.get_constants locs in
    let fp_sort = Set.mk_sort locs_sort in
    let global_fp = Set.mk_var "footprint0" fp_sort in
    let heap_sort = Array.mk_sort locs_sort locs_sort in
    let heap = Array.mk_var "heap" heap_sort in
    let heap_p = Array.mk_var "prev" heap_sort in
    (Context.init info locs_sort loc_consts global_fp heap heap_p, locs)

  (* ==== Helper functions for constructing common terms ==== *)

  let mk_heap_succ (context : Context.t) x = Array.mk_select context.heap x

  let mk_is_heap_succ context x y =
    let select = mk_heap_succ context x in
    Boolean.mk_eq select y

  let mk_range context domain range =
    Locations.mk_forall context.locs_sort
      (fun l ->
        let in_domain = Set.mk_mem l domain in
        let succ_in_range = Set.mk_mem (mk_heap_succ context l) range in
        Boolean.mk_iff in_domain succ_in_range
      )

  let term_to_expr context (SSL.Var x) = SMT.Variable.mk (SSL.Variable.show x) context.locs_sort
  let var_to_expr context x = term_to_expr context (SSL.Var x)

  let mk_strongly_disjoint context footprint1 footprint2 =
    let range1 = Set.mk_fresh_var "range" context.fp_sort in
    let range2 = Set.mk_fresh_var "range" context.fp_sort in

    let ls1 = Set.mk_fresh_var "locs" context.fp_sort in
    let ls2 = Set.mk_fresh_var "locs" context.fp_sort in

    let is_range1 = mk_range context footprint1 range1 in
    let is_range2 = mk_range context footprint2 range2 in

    let locs1 = Set.mk_union [footprint1; range1] context.fp_sort in
    let locs2 = Set.mk_union [footprint2; range2] context.fp_sort in

    let common_locs = Set.mk_inter [locs1; locs2] context.fp_sort in
    let variables = List.map (var_to_expr context) context.vars in
    let stack_image = Set.mk_enumeration context.fp_sort variables in
    let strongly_disjoint = Set.mk_subset common_locs stack_image in

    let axioms = Boolean.mk_and [is_range1; is_range2] in
    (axioms, strongly_disjoint)

  (** Create predicate \forall x \in fp. h1[x] = h2[x] *)
  let heaps_equal_on_footprint context h1 h2 fp =
    Locations.mk_forall context.locs_sort
      (fun loc ->
        let in_fp = Set.mk_mem loc fp in
        let h1_select = Array.mk_select h1 loc in
        let h2_select = Array.mk_select h2 loc in
        let select_eq = Boolean.mk_eq h1_select h2_select in
        Boolean.mk_implies in_fp select_eq
      )

  let formula_footprint context phi =
    Set.mk_var (Context.formula_footprint context phi) context.fp_sort

  (* ==== Recursive translation of SL formulae ==== *)

  let rec translate context phi domain = match phi with
    | SSL.PointsTo (x, [y]) -> translate_pointsto context domain x y
    | SSL.PointsTo (x, ys) -> translate_dpointsto context domain x ys
    | SSL.And (psi1, psi2) -> translate_and context domain psi1 psi2
    | SSL.Or (psi1, psi2) -> translate_or context domain psi1 psi2
    | SSL.Star (psi1, psi2) -> translate_star context domain psi1 psi2
    | SSL.Septraction (psi1, psi2) -> translate_septraction context domain phi psi1 psi2
    | SSL.Eq (Var var1, Var var2) -> translate_eq context domain var1 var2
    | SSL.Neq (Var var1, Var var2) -> translate_neq context domain var1 var2
    | SSL.LS (Var var1, Var var2) -> translate_ls context domain var1 var2
    | SSL.DLS (Var x, Var y, Var f, Var l) -> translate_dls context domain x y f l
    | SSL.SkipList (2, Var x, Var y) -> translate_skl context domain x y
    | SSL.Not (psi) -> translate_not context domain psi
    | SSL.GuardedNeg (psi1, psi2) -> translate_guarded_neg context domain psi1 psi2
    | SSL.Pure term -> translate_pure context domain term

  and translate_pure context domain term =
    let semantics = term in
    let axioms = Boolean.mk_true () in
    let footprints = [Set.mk_empty context.fp_sort] in
    (semantics, axioms, footprints)

  and translate_pointsto context domain x y =
    let x = term_to_expr context x in
    let y = term_to_expr context y in

    let domain_def = Set.mk_eq_singleton domain x in
    let pointer = mk_is_heap_succ context x y in
    let semantics = Boolean.mk_and [pointer; domain_def] in
    let axioms = Boolean.mk_true () in
    let footprints = [Set.mk_singleton x] in

    (semantics, axioms, footprints)

  (* FIXME: this is only first idea assuming implicit order on selectors. *)
  and translate_dpointsto context domain x [n; p] =
    let x = term_to_expr context x in
    let n = term_to_expr context n in
    let p = term_to_expr context p in

    let domain_def = Set.mk_eq_singleton domain x in
    let pointer_next = mk_is_heap_succ context x n in
    let pointer_prev = mk_is_heap_succ {context with heap = context.heap_prev} x p in

    let semantics = Boolean.mk_and [pointer_next; pointer_prev; domain_def] in
    let axioms = Boolean.mk_true () in
    let footprints = [Set.mk_singleton x] in

    (semantics, axioms, footprints)


  and translate_ls context domain x y =
    let local_bound = Bounds.local_bound context x y in
    let x = var_to_expr context x in
    let y = var_to_expr context y in

    let fp = SMT.Set.mk_fresh_var "ls_fp" context.fp_sort in
    let semantics = ListEncoding.semantics context domain x y local_bound in
    let axioms = ListEncoding.axioms context fp x y local_bound in
    let footprints = [fp] in
    (semantics, axioms, footprints)

  and translate_dls context domain x y f l =
    let local_bound = (0, context.location_bound - 1) in (*TODO: more precise bounds*)
    let x = var_to_expr context x in
    let y = var_to_expr context y in
    let f = var_to_expr context f in
    let l = var_to_expr context l in

    let fp = SMT.Set.mk_fresh_var "dls_fp" context.fp_sort in
    let semantics = DLListEncoding.semantics context domain fp x y f l local_bound in
    let axioms = DLListEncoding.axioms context fp x y f l local_bound in
    let footprints = [fp] in
    (semantics, axioms, footprints)

  and translate_skl context domain x y =
    let local_bound = (0, context.location_bound - 1) in (*TODO: more precise bounds*)
    let x = var_to_expr context x in
    let y = var_to_expr context y in

    let fp1 = SMT.Set.mk_fresh_var "skl_fp1" context.fp_sort in
    let fp2 = SMT.Set.mk_fresh_var "skl_fp2" context.fp_sort in
    let semantics = SkipListEncoding.semantics context domain fp1 fp2 x y local_bound in
    let axioms = SkipListEncoding.axioms context fp1 fp2 x y local_bound in
    let footprints = [fp1] in
    (semantics, axioms, footprints)

  and translate_eq context domain x y =
    let x = var_to_expr context x in
    let y = var_to_expr context y in

    let domain_def = Set.mk_eq_empty domain in
    let equals = Boolean.mk_eq x y in
    let semantics = Boolean.mk_and [equals; domain_def] in
    let axioms = Boolean.mk_true () in
    let footprints = [Set.mk_empty context.fp_sort] in
    (semantics, axioms, footprints)

  and translate_neq context domain x y =
    let x = var_to_expr context x in
    let y = var_to_expr context y in

    let domain_def = Set.mk_eq_empty domain in
    let distinct = Boolean.mk_distinct [x; y] in
    let semantics = Boolean.mk_and [distinct; domain_def] in
    let axioms = Boolean.mk_true () in
    let footprints = [Set.mk_empty context.fp_sort] in
    (semantics, axioms, footprints)

  and translate_not context domain phi =
    let context = {context with
      polarity = not context.polarity;
      can_skolemise = false
    }
    in

    let psi, axioms, footprints = translate context  phi domain in

    let semantics = Boolean.mk_not psi in
    let footprints =
      if (not @@ context.under_star) || (Locations.name = "bitvectors")
      then
        []
      else
        Context.locations_powerset context
        |> List.map (Set.mk_enumeration context.fp_sort)

    in
    (semantics, axioms, footprints)

  and translate_and context domain psi1 psi2 =
    let phi1, axioms1, footprints1 = translate context psi1 domain in
    let phi2, axioms2, footprints2 = translate context psi2 domain in

    let semantics = Boolean.mk_and [phi1; phi2] in
    let axioms = Boolean.mk_and [axioms1; axioms2] in
    (semantics, axioms, footprints1)

  and translate_or context domain psi1 psi2 =
    let phi1, axioms1, footprints1 = translate context psi1 domain in
    let phi2, axioms2, footprints2 = translate context psi2 domain in

    let semantics = Boolean.mk_or [phi1; phi2] in
    let axioms = Boolean.mk_and [axioms1; axioms2] in
    let footprints = footprints1 @ footprints2 in
    (semantics, axioms, footprints)

  (** Translation of separating conjunction using skolemisation. *)
  and translate_star_skolemised context domain psi1 psi2 =
    let fp1 = formula_footprint context psi1 in
    let fp2 = formula_footprint context psi2 in

    let semantics1, axioms1, footprints1 = translate context psi1 fp1 in
    let semantics2, axioms2, footprints2 = translate context psi2 fp2 in
    let disjoint = Set.mk_disjoint fp1 fp2 in
    let fp_union = Set.mk_union [fp1; fp2] context.fp_sort in
    let domain_def = Set.mk_eq domain fp_union in

    let axioms = Boolean.mk_and [axioms1; axioms2] in

    (* Classical semantics for positive formulae *)
    if SSL.is_positive psi1 && SSL.is_positive psi2
       || not @@ Options.strong_separation ()
    then
      (Boolean.mk_and [semantics1; semantics2; disjoint; domain_def], axioms, [])

    (* Strong-separating semantics *)
    else
      let axiom, str_disjoint = mk_strongly_disjoint context fp1 fp2 in
      let axioms = Boolean.mk_and [axioms; axiom] in
      (Boolean.mk_and [semantics1; semantics2; disjoint; str_disjoint; domain_def], axioms, [])

  (** Translation of separating conjunction using second-order quantifiers. *)
  and translate_star_quantified context domain psi1 psi2 =
    (* Quantifier binders *)
    let fp1 = SMT.Set.mk_fresh_var "FP1" context.fp_sort in
    let fp2 = SMT.Set.mk_fresh_var "FP2" context.fp_sort in

    let semantics1, axioms1, footprints1 = translate context psi1 fp1 in
    let semantics2, axioms2, footprints2 = translate context psi2 fp2 in

    (* Create lists of possible footprints. *)
    let fp_worklist =
      BatList.cartesian_product footprints1 footprints2
      |> List.filter (fun (fp1, fp2) -> SMT.Set.may_disjoint fp1 fp2)
    in

    (* Semantics *)
    let disjoint = Set.mk_disjoint fp1 fp2 in
    let fp_union = Set.mk_union [fp1; fp2] context.fp_sort in
    let domain_def = Set.mk_eq domain fp_union in

    let axioms = Boolean.mk_and [axioms1; axioms2] in
    let footprints =
      fp_worklist
      |> List.map (fun (s1, s2) -> Set.mk_union [s1; s2] context.fp_sort)
    in

    (* Unique footprints *)
    if List.length footprints1 == 1 && List.length footprints2 == 1 then
      let semantics1 = SMT.substitute semantics1 fp1 (List.hd footprints1) in
      let semantics2 = SMT.substitute semantics2 fp2 (List.hd footprints2) in
      let disjoint = Set.mk_disjoint (List.hd footprints1) (List.hd footprints2) in
      let fp_union = Set.mk_union [List.hd footprints1; List.hd footprints2] context.fp_sort in
      let domain_def = Set.mk_eq domain fp_union in
      let semantics = Boolean.mk_and [semantics1; semantics2; disjoint; domain_def] in
      (semantics, axioms, footprints)

    (* Not unique footprint, but positive (eg., entailment with disjunctions) *)
    else if SSL.is_positive psi1 && SSL.is_positive psi2
            || not @@ Options.strong_separation ()
    then
      let semantics =
        Boolean.mk_and [semantics1; semantics2; disjoint; domain_def]
        |> Quantifier.mk_exists2 [fp1; fp2] [footprints1; footprints2]
      in
      let axioms = Boolean.mk_and [axioms1; axioms2] in
      (semantics, axioms, footprints)

    (* Strong-separating semantics *)
    else
      let ssl_axiom, str_disjoint = mk_strongly_disjoint context fp1 fp2 in
      let semantics =
        Boolean.mk_and [semantics1; semantics2; disjoint; domain_def; str_disjoint]
        |> Quantifier.mk_exists2 [fp1; fp2] [footprints1; footprints2]
      in
      let ssl_axioms1 = Quantifier.mk_forall2 [fp1] [footprints1] ssl_axiom in
      let ssl_axioms2 = Quantifier.mk_forall2 [fp2] [footprints2] ssl_axiom in
      let axioms = Boolean.mk_and [axioms1; axioms2; ssl_axioms1; ssl_axioms2] in
      (semantics, axioms, footprints)

  (** Generic translation of separating conjunction. *)
  and translate_star context domain psi1 psi2 =
    let context = {context with under_star = not context.can_skolemise} in
    if context.can_skolemise
    then translate_star_skolemised context domain psi1 psi2
    else translate_star_quantified context domain psi1 psi2

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

  (** Translation of septractions *)
  and translate_septraction (context : Context.t) domain phi psi1 psi2 =
    let h1_name = Context.formula_witness_heap context phi in
    let h1 = Array.mk_var h1_name context.heap_sort in

    let fp1 = formula_footprint context psi1 in
    let fp2 = formula_footprint context psi2 in
    let fp = Set.mk_diff fp2 fp1 in

    let phi1, axioms1, footprints1 = translate {context with heap = h1} psi1 fp1 in
    let phi2, axioms2, footprints2 = translate {context with heap = h1} psi2 fp2 in

    let eq_fp = heaps_equal_on_footprint context context.heap h1 fp in

    let subset = Set.mk_subset fp1 fp2 in
    let domain_def = Set.mk_eq domain fp in
    let axioms = Boolean.mk_and [axioms1; axioms2] in
    let semantics = Boolean.mk_and [phi1; phi2; subset; domain_def; eq_fp] in
    let footprints =
      BatList.cartesian_product footprints1 footprints2
      |> List.map (fun (s1, s2) -> Set.mk_diff s2 s1)
    in

    (* Positive septraction *)
    if context.can_skolemise then
      (semantics, axioms, footprints)

    (* Negative septraction in WSL *)
    else if SSL.has_unique_shape psi1 && not @@ Options.strong_separation () then
      (semantics, axioms, footprints)

    (* Negative septraction *)
    else if SSL.has_unique_shape psi1 then
      let axiom, strongly_disjoint = mk_strongly_disjoint context fp1 domain in
      let semantics = Boolean.mk_and [semantics; strongly_disjoint] in
      let axioms = Boolean.mk_and [axioms; axiom] in
      (semantics, axioms, footprints)

    else failwith "Not supported form of septraction/magic wand"

let generate_heap_axioms context heap locs =
  List.map
    (fun x ->
      let hx = Array.mk_select heap x in
      Locations.var_axiom locs hx
    ) context.locs
  |> Boolean.mk_and

let translate_phi (context : Context.t) locs ssl_phi =
  let footprint = formula_footprint context ssl_phi in
  let phi, axioms, _ = translate context ssl_phi context.global_footprint in
  let nil = SMT.Variable.mk "nil" context.locs_sort in
  let nil_not_in_fp = Boolean.mk_not (Set.mk_mem nil context.global_footprint) in

  let heap_nil = Array.mk_select context.heap nil in
  let heap_intro = Boolean.mk_eq nil heap_nil in

  (* Variable constraints *)
  let var_constraints =
    List.map (fun v -> Locations.var_axiom locs @@ var_to_expr context v) (SSL.Variable.nil :: context.vars)
    |> Boolean.mk_and
  in

  let heaps =
    SSL.select_subformulae (fun x -> match x with Septraction _ -> true | _ -> false) ssl_phi
    |> List.map (fun psi -> Format.asprintf "heap%d" (SSL.subformula_id ssl_phi psi))
    |> List.map (fun name -> Array.mk_var name context.heap_sort)
    |> List.append [context.heap]
  in

  (* Heap constraints *)
  let heap_constraints =
    List.map (fun h -> generate_heap_axioms context h locs) heaps
    |> Boolean.mk_and
  in

  let location_lemmas = Locations.location_lemmas locs in

  Boolean.mk_and
    [phi; axioms; heap_intro; nil_not_in_fp; location_lemmas; heap_constraints; var_constraints]

  (* ==== Translation of SMT model to stack-heap model ==== *)

  let nil_interp context model =
    let e = SMT.Variable.mk (SSL.Variable.show SSL.Variable.nil) context.locs_sort in
    try Model.eval model e
    with _ -> failwith "No interpretation of nil"

  let translate_loc loc = match loc with
    | SMT.IntConst i -> i
    | SMT.BitConst (i, _) -> i
    | SMT.Constant (c, _) ->
        begin
          try int_of_string c
          with _ ->
          begin
            try int_of_string @@ BatString.chop ~l:1 ~r:1 c
            with _ -> failwith ("Cannot translate location " ^ SMT.show_with_sort loc)
          end
        end
    | other -> failwith ("Cannot translate location " ^ SMT.show_with_sort other)

  let translate_stack context model =
    List.fold_left
      (fun stack var ->
        let var_expr = SMT.Variable.mk (SSL.Variable.show var) context.locs_sort in
        let loc =
          try Model.eval model var_expr
          with Not_found -> nil_interp context model
        in
        Stack.add var (translate_loc loc) stack
      ) Stack.empty (SSL.Variable.nil :: context.vars)

  let translate_heap context model heap_term fp =
    let fp = SMT.Set.get_elems @@ Model.eval model fp in
    List.fold_left
      (fun heap loc ->
        let loc_name = translate_loc loc in
        let heap_image = Array.mk_select heap_term loc in
        let image_name =
          Model.eval model heap_image
          |> translate_loc
        in
        let heap = Heap.add_next loc_name image_name heap in
        (* TODO: handle prev properly *)
        let heap_image_prev = Array.mk_select context.heap_prev loc in
        let image_name_prev =
          Model.eval model heap_image_prev
          |> translate_loc
        in
        Heap.add_prev loc_name image_name_prev heap
      ) Heap.empty fp

  (** TODO: translate ALL footprint symbols introduced during translation. *)
  let translate_footprints context model =
    List.fold_left
      (fun acc (psi, fp_expr) ->
        let set = SMT.Set.get_elems @@ Model.eval model fp_expr in
        let fp = Footprint.of_list @@ List.map translate_loc set in
        SSL.Map.add psi fp acc
      ) SSL.Map.empty [(context.phi, context.global_footprint)]

  (* TODO: translation of witness heaps *)
  let translate_witness_heaps context model = SSL.Map.empty
  (*
    SSL.fold
      (fun phi acc -> match phi with
        | Septraction (_, psi2) ->
          let id = SSL.subformula_id context.phi phi in
          let heap_name = Format.asprintf "heap%d" id in
          let heap_expr = Var.mk heap_name context.heap_sort in
          let id = SSL.subformula_id context.phi psi2 in
          let fp_name = Format.asprintf "footprint%d" id in
          let fp_expr = Var.mk fp_name context.fp_sort in
          let heap = translate_heap context model heap_expr fp_expr in
          SSL.Map.add phi heap acc
        | _ -> acc
      ) context.phi SSL.Map.empty
  *)

  let translate_model context model =
    let s = translate_stack context model in
    let h = translate_heap context model context.heap context.global_footprint in
    let footprints = translate_footprints context model in
    let heaps = translate_witness_heaps context model in
    StackHeapModel.init ~heaps s h

  (* ==== Solver ==== *)
  let solve input =
    let context, locs = init input in
    Print.debug "Translating
     - Location encoding: %s
     - Set encoding: %s
     - Backend: %s\n
     - Stack bound: [%d, %d]
     - Location bound:  %d\n"
     Locations.name
     Encoding.Set.name
     Backend.name
     (fst input.stack_bound)
     (snd input.stack_bound)
     input.location_bound
     ;

    (* Translation *)
    let translated1 = translate_phi context locs input.phi in
    Debug.translated ~suffix:"1" translated1;

    (* Set rewritting *)
    let translated2 = Encoding.Set.rewrite translated1 in
    Debug.translated ~suffix:"2" translated2;

    (* Quantifier elimination *)
    let translated = Options.quantif_elim () translated2 in
    Debug.translated ~suffix:"3" translated;


    let size = SMT.Term.size translated in
    let input = Input.set_statistics ~size:(Some size) input in

    Debug.context context;

    Print.debug "Running backend SMT solver\n";
    Timer.add "Astral";

    Backend.init ();

    Debug.backend_translated (Backend.show_formula @@ Backend.translate translated);
    Debug.backend_simplified (Backend.show_formula @@ Backend.simplify @@ Backend.translate translated);
    Debug.backend_smt_benchmark (Backend.to_smt_benchmark @@ Backend.translate translated);

    (* Solve *)
    let produce_models = input.get_model || Options.produce_models () in
    match Backend.solve translated produce_models with
    | SMT_Sat None -> Input.set_result `Sat input
    | SMT_Sat (Some (smt_model, backend_model)) ->
      let _ = Debug.backend_model (Backend.show_model backend_model) in
      let _ = Debug.smt_model smt_model in
      let sh = translate_model context smt_model in
      let _ = Debug.model sh in
      Input.set_result `Sat ~model:(Some sh) input

    (* TODO: unsat cores *)
    | SMT_Unsat unsat_core -> Input.set_result `Unsat ~unsat_core:(Some []) input

    | SMT_Unknown reason -> Input.set_result `Unknown ~reason:(Some reason) input
end
