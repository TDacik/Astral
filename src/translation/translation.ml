(* Translation of SSL formulae to SMT.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open StackHeapModel

module Input = Context
module Context = Translation_context

module Printer = Printer.Make(struct let name = "Translation" end)

exception UnsupportedFragment

module Make (Encoding : Translation_sig.ENCODING) (Backend : Backend_sig.BACKEND) = struct

  open Encoding
  open SMT
  open Context

  module DLListEncoding = DLListEncoding.Make(Encoding.Locations)
  module NestedListEncoding = NestedListEncoding.Make(Encoding.Locations)

  module Footprints = Topped_set.Lift(SMT.Term.Collections.Set)

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

  let mk_image context domain =
    let location_term l =
      Boolean.mk_ite
        (Set.mk_mem l domain)
        (Set.mk_singleton @@ mk_heap_succ context l)
        (Set.mk_empty context.fp_sort)
    in
    let terms = List.map location_term context.locs in
    Set.mk_union terms context.fp_sort

  let mk_locs context domain = Set.mk_union [mk_image context domain; domain] context.fp_sort

  let term_to_expr context t = match t with
    | SSL.Var x -> SMT.Variable.mk (SSL.Variable.show x) context.locs_sort
    | other -> failwith ("Cannot translate term " ^ SSL.show other)

  let var_to_expr context x = term_to_expr context (SSL.Var x)

  let mk_strongly_disjoint context domains =
    let locs = List.map (mk_locs context) domains in
    let worklist =
      List_utils.diagonal_product locs
      |> List.map (fun (locs1, locs2) -> Set.mk_inter [locs1; locs2] context.fp_sort)
    in
    let common_locs = Set.mk_union worklist context.fp_sort in
    let variables = List.map (var_to_expr context) context.vars in
    let stack_image = Set.mk_enumeration context.fp_sort variables in
    Set.mk_subset common_locs stack_image

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
    | SSL.Emp -> translate_emp context domain
    | SSL.PointsTo (x, [y]) -> translate_pointsto context domain x y
    | SSL.PointsTo (x, ys) -> translate_dpointsto context domain x ys
    | SSL.And (psi1, psi2) -> translate_and context domain psi1 psi2
    | SSL.Or (psi1, psi2) -> translate_or context domain psi1 psi2
    | SSL.Star psis -> translate_star context domain psis
    | SSL.Septraction (psi1, psi2) -> translate_septraction context domain phi psi1 psi2
    | SSL.Eq xs -> translate_eq context domain xs
    | SSL.Distinct xs -> translate_distinct context domain xs
    | SSL.LS (x, y) -> translate_ls context domain x y
    | SSL.DLS (x, y, f, l) -> translate_dls context domain x y f l
    | SSL.NLS (x, y, z) -> translate_nls context domain x y z
    | SSL.SkipList (2, x, y) -> translate_skl context domain x y
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
    let x = term_to_expr context x in
    let y = term_to_expr context y in

    let domain_def = Set.mk_eq_singleton domain x in
    let pointer = mk_is_heap_succ context x y in
    let semantics = Boolean.mk_and [pointer; domain_def] in
    let axioms = Boolean.mk_true () in

    let fp = Set.mk_singleton x in
    let footprints = Footprints.singleton fp in

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

    let fp = Set.mk_singleton x in
    let footprints = Footprints.singleton fp in

    (semantics, axioms, footprints)


  and translate_ls context domain (Var x) (Var y) =
    let local_bound = Bounds.list_bound context x y in
    let x = var_to_expr context x in
    let y = var_to_expr context y in

    let fp = SMT.Set.mk_fresh_var "ls_fp" context.fp_sort in
    let semantics = ListEncoding.semantics context domain x y local_bound in
    let axioms = ListEncoding.axioms context fp x y local_bound in
    let footprint = ListEncoding.footprints context fp x y local_bound in
    (semantics, axioms, Footprints.singleton footprint)

  and translate_dls context domain (Var x) (Var y) (Var f) (Var l) =
    let local_bound = (0, context.location_bound - 1) in (*TODO: more precise bounds*)
    let x = var_to_expr context x in
    let y = var_to_expr context y in
    let f = var_to_expr context f in
    let l = var_to_expr context l in

    let fp = SMT.Set.mk_fresh_var "dls_fp" context.fp_sort in
    let semantics = DLListEncoding.semantics context domain fp x y f l local_bound in
    let axioms = DLListEncoding.axioms context fp x y f l local_bound in
    let footprints = Footprints.singleton fp in
    (semantics, axioms, footprints)

  and translate_nls context domain (Var x) (Var y) (Var z) =
    let local_bound = (0, context.location_bound - 1) in (*TODO: more precise bounds*)
    let x = var_to_expr context x in
    let y = var_to_expr context y in
    let z = var_to_expr context z in

    let fp = SMT.Set.mk_fresh_var "nls_fp" context.fp_sort in
    let semantics = NestedListEncoding.semantics context domain fp x y z local_bound in
    let axioms = NestedListEncoding.axioms context fp x y z local_bound in
    let footprints = Footprints.singleton fp in
    (semantics, axioms, footprints)

  and translate_skl context domain (Var x) (Var y) =
    let local_bound = (0, context.location_bound - 1) in (*TODO: more precise bounds*)
    let x = var_to_expr context x in
    let y = var_to_expr context y in

    let fp1 = SMT.Set.mk_fresh_var "skl_fp1" context.fp_sort in
    let fp2 = SMT.Set.mk_fresh_var "skl_fp2" context.fp_sort in
    let semantics = SkipListEncoding.semantics context domain fp1 fp2 x y local_bound in
    let axioms = SkipListEncoding.axioms context fp1 fp2 x y local_bound in
    let footprints = Footprints.singleton fp1 in
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
    (semantics, axioms, footprints1)

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
      if List.for_all SSL.is_positive psis || not @@ Options.strong_separation ()
      then Boolean.mk_true ()
      else mk_strongly_disjoint context fps
    in
    (Boolean.mk_and (semantics @ [disjoint; domain_def; str_disjoint]), axioms, footprints)

  (** Translation of separating conjunction using second-order quantifiers. *)
  and translate_star_quantified context domain psis =
    (* Quantifier binders *)
    let fps =
      List.mapi (fun i _ -> Format.asprintf "FP%d" i) psis
      |> List.map (fun v -> SMT.Set.mk_fresh_var v context.fp_sort)
    in

    let semantics, axioms, footprints =
      List.map2 (translate context) psis fps
      |> List_utils.split3
    in

    (* Create lists of possible footprints. *)
    let fp_worklist = Footprints.apply_partial_variadic_op
      (fun fps ->
        (* TODO: if SMT.Set.may_disjoint fp1 fp2 *)
        Some (SMT.Set.mk_union fps context.fp_sort)
      ) footprints
    in

    let axioms = Boolean.mk_and axioms in

    (* Unique footprints *)
    if List.for_all SSL.has_unique_footprint psis then begin
      assert (List.for_all (fun fp -> Footprints.cardinal fp == 1) footprints);
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
        if List.for_all SSL.is_positive psis || not @@ Options.strong_separation ()
        then Boolean.mk_true ()
        else mk_strongly_disjoint context fps
      in
      let disjoint = Set.mk_disjoint_list fps in
      let fp_union = Set.mk_union fps context.fp_sort in
      let domain_def = Set.mk_eq domain fp_union in
      let ranges =
        try Some (List.map Footprints.elements footprints)
        with Footprints.TopError -> None
      in
      let semantics =
        Boolean.mk_and (disjoint :: domain_def :: str_disjoint :: semantics)
        |> Quantifier.mk_exists2 fps ~ranges
      in
      let footprints = List.fold_left Footprints.union Footprints.empty footprints in
      (semantics, axioms, footprints)

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

  (** Generic translation of septraction. *)
  and translate_septraction context domain phi psi1 psi2 =
    let h1_name = Context.formula_witness_heap context phi in
    let h1 = Array.mk_var h1_name context.heap_sort in

    if context.can_skolemise
    then translate_septraction_skolemised context domain h1 phi psi1 psi2
    else raise UnsupportedFragment
    (*  if SSL.has_unique_shape psi1
      then translate_septraction_quantified context domain h1 phi psi1 psi2
      else raise UnsupportedFragment
    *)

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

    let semantics =
      context.locs
      |> List.map (SMT.substitute semantics x)
      |> Boolean.mk_or
    in

    let footprints =
      context.locs
      |> List.map
          (fun loc ->
            Footprints.map
              (fun fp ->
                SMT.substitute fp x loc
              ) footprints
          )
      |> List.fold_left Footprints.union Footprints.empty
    in

    (* TODO: check footprint *)
    (semantics, axioms, footprints)

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
    List.map
      (fun v ->
        Locations.var_axiom locs @@ var_to_expr context v
      ) (SSL.Variable.nil :: context.vars)
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
            try int_of_string @@ BatString.chop ~l:4 ~r:0 c
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
    (* TODO: handle prev properly *)
    List.fold_left
      (fun heap loc ->
        try
          let loc_name = translate_loc loc in
          let heap_image = Array.mk_select heap_term loc in
          let image_name =
            Model.eval model heap_image
            |> translate_loc
          in
          let heap_image_prev = Array.mk_select context.heap_prev loc in
          let image_name_prev =
            Model.eval model heap_image_prev
            |> translate_loc
          in
          Heap.add loc_name [image_name; image_name_prev] heap
        with _ ->
          let loc_name = translate_loc loc in
          let heap_image = Array.mk_select heap_term loc in
          let image_name =
            Model.eval model heap_image
            |> translate_loc
          in
          Heap.add_next loc_name image_name heap
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
    Printer.debug "Translating
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
    let translated3 = Options.quantif_elim () context translated2 in
    Debug.translated ~suffix:"3" translated3;

    (* Backend preprocessor *)
    let translated = Backend_preprocessor.apply translated3 in
    Debug.translated ~suffix:"4" translated;

    let size = SMT.Term.size translated in
    let input = Input.set_statistics ~size:(Some size) input in

    Debug.context context;

    let produce_models = input.get_model || Options.produce_models () in
    let user_options = Options.backend_options () in

    (* Backend debugging *)
    Backend.init ();
    let backend_translated = Backend.translate translated in
    Debug.backend_translated (Backend.show_formula backend_translated);
    Debug.backend_simplified (Backend.show_formula @@ Backend.simplify backend_translated);
    Debug.backend_input (Backend.to_smtlib translated produce_models user_options);

    Printer.debug "Running backend SMT solver\n";
    Timer.add "Translation";

    (* Solve *)
    match Backend.solve context translated produce_models user_options with
    | SMT_Sat None -> Input.set_result `Sat input
    | SMT_Sat (Some (smt_model, backend_model)) ->
      let _ = Debug.backend_model (Backend.show_model backend_model) in
      let smt_model = Encoding.Set.rewrite_back translated1 smt_model in
      let _ = Debug.smt_model smt_model in
      let sh = translate_model context smt_model in
      let _ = Debug.model sh in
      Input.set_result `Sat ~model:(Some sh) input

    (* TODO: unsat cores *)
    | SMT_Unsat unsat_core -> Input.set_result `Unsat ~unsat_core:(Some []) input

    | SMT_Unknown reason -> Input.set_result `Unknown ~reason:(Some reason) input

  let solve input =
    try solve input
    with UnsupportedFragment ->
      Input.set_result `Unknown ~reason:(Some "Unsupported fragment") input

end
