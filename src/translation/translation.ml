(* Translation of SL formulae to SMT
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Batteries

open Results
open Context
open StackHeapModel

type result =
  | Sat of StackHeapModel.t * Results.t
  | Unsat of Results.t * SMT.term list
  | Unknown of Results.t * string

module Print = Printer.Make(struct let name = "Translation" end)

module Make (Encoding : Translation_sig.ENCODING) (Backend : Backend_sig.BACKEND) = struct

  open Encoding
  open SMT

  (** Initialization of Z3 context and translation context *)
  let init phi info =
    let locs_card = info.heap_bound + 1 in (* one for nil *)
    let locs_sort = Locations.mk_sort "Loc" locs_card in
    let locs = Locations.enumeration locs_sort in
    let fp_sort = Set.mk_sort locs_sort in
    let global_fp = Set.mk_var "global fp" fp_sort in
    let heap_sort = Array.mk_sort locs_sort locs_sort in
    let heap = Array.mk_var "heap" heap_sort in
    Context.init info locs_sort locs fp_sort global_fp heap_sort heap

  (* ==== Helper functions for constructing common terms ==== *)

  let mk_heap_succ (context : Context.t) x = Array.mk_select context.heap x

  let mk_is_heap_succ context x y =
    let select = mk_heap_succ context x in
    Boolean.mk_eq select y

  let mk_range context domain range =
    Locations.forall context
      (fun l ->
        let in_domain = Set.mk_mem l domain in
        let succ_in_range = Set.mk_mem (mk_heap_succ context l) range in
        Boolean.mk_iff in_domain succ_in_range
      )

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
    let variables = Locations.vars_to_exprs context in
    let stack_image = Set.mk_enumeration context.fp_sort variables in
    let strongly_disjoint = Set.mk_subset common_locs stack_image in

    let axioms = Boolean.mk_and [is_range1; is_range2] in
    (axioms, strongly_disjoint)

  (** Create predicate \forall x \in fp. h1[x] = h2[x] *)
  let heaps_equal_on_footprint context h1 h2 fp =
    Locations.forall context
      (fun loc ->
        let in_fp = Set.mk_mem loc fp in
        let h1_select = Array.mk_select h1 loc in
        let h2_select = Array.mk_select h2 loc in
        let select_eq = Boolean.mk_eq h1_select h2_select in
        Boolean.mk_implies in_fp select_eq
      )

  let term_to_expr context x = match x with
    | SSL.Variable.Var _ | SSL.Variable.Nil -> Locations.var_to_expr context x
    | SSL.Variable.Term t -> t

  let formula_footprint context phi =
    Set.mk_var (Context.formula_footprint context phi) context.fp_sort

  (* ==== Recursive translation of SL formulae ==== *)

  let rec translate context phi domain = match phi with
    | SSL.PointsTo (var1, var2) -> translate_pointsto context domain var1 var2
    | SSL.And (psi1, psi2) -> translate_and context domain psi1 psi2
    | SSL.Or (psi1, psi2) -> translate_or context domain psi1 psi2
    | SSL.Star (psi1, psi2) -> translate_star context domain psi1 psi2
    | SSL.Septraction (psi1, psi2) -> translate_septraction context domain phi psi1 psi2
    | SSL.Eq (var1, var2) -> translate_eq context domain var1 var2
    | SSL.Neq (var1, var2) -> translate_neq context domain var1 var2
    | SSL.LS (var1, var2) -> translate_ls context domain var1 var2
    | SSL.Not (psi) -> translate_not context domain psi
    | SSL.GuardedNeg (psi1, psi2) -> translate_guarded_neg context domain psi1 psi2

  and translate_pointsto context domain x y =
    let x = Locations.var_to_expr context x in
    let y = Locations.var_to_expr context y in

    let domain_def = Set.mk_eq_singleton domain x in
    let pointer = mk_is_heap_succ context x y in
    let semantics = Boolean.mk_and [pointer; domain_def] in
    let axioms = Boolean.mk_true () in
    let footprints = [Set.mk_singleton x] in

    (semantics, axioms, footprints)

  and translate_ls context domain x y =
    let local_bound = Bounds.local_bound context x y in
    let x = Locations.var_to_expr context x in
    let y = Locations.var_to_expr context y in

    let fp = SMT.Set.mk_fresh_var "list_fp" context.fp_sort in
    let semantics = ListEncoding.semantics context domain x y local_bound in
    let axioms = ListEncoding.axioms context fp x y local_bound in
    let footprints = [fp] in
    (semantics, axioms, footprints)

  and translate_eq context domain x y =
    let x = term_to_expr context x in
    let y = term_to_expr context y in

    let domain_def = Set.mk_eq_empty domain in
    let equals = Boolean.mk_eq x y in
    let semantics = Boolean.mk_and [equals; domain_def] in
    let axioms = Boolean.mk_true () in
    let footprints = [Set.mk_empty context.fp_sort] in
    (semantics, axioms, footprints)

  and translate_neq context domain x y =
    let x = term_to_expr context x in
    let y = term_to_expr context y in

    let domain_def = Set.mk_eq_empty domain in
    let distinct = Boolean.mk_distinct [x; y] in
    let semantics = Boolean.mk_and [distinct; domain_def] in
    let axioms = Boolean.mk_true () in
    let footprints = [Set.mk_empty context.fp_sort] in
    (semantics, axioms, footprints)

  and translate_not context domain phi =
    let context = {context with
      polarity = not context.polarity;
      can_scolemise = false
    }
    in

    let psi, axioms, footprints = translate context  phi domain in

    let semantics = Boolean.mk_not psi in
    let footprints =
      if not @@ context.under_star
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

  and translate_star context domain psi1 psi2 =
    let context = {context with under_star = not context.can_scolemise} in
    if context.can_scolemise then begin
      let fp1 = formula_footprint context psi1 in
      let fp2 = formula_footprint context psi2 in
      let phi1, axioms1, footprints1 = translate context psi1 fp1 in
      let phi2, axioms2, footprints2 = translate context psi2 fp2 in
      let disjoint = Set.mk_disjoint fp1 fp2 in
      let fp_union = Set.mk_union [fp1; fp2] context.fp_sort in
      let domain_def = Set.mk_eq domain fp_union in
      let axioms = Boolean.mk_and [axioms1; axioms2] in
      if SSL.has_unique_footprint psi1 && SSL.has_unique_footprint psi2
         || not @@ Options.strong_separation ()
      then
        (Boolean.mk_and [phi1; phi2; disjoint; domain_def], axioms, [])
      else
       let axiom, str_disjoint = mk_strongly_disjoint context fp1 fp2 in
       let axioms = Boolean.mk_and [axioms; axiom] in
       (Boolean.mk_and [phi1; phi2; disjoint; str_disjoint; domain_def], axioms, [])
    end
    (* Generation of axioms for each footprint *)
    else begin
      let phi1, axioms1, footprints1 = translate context psi1 domain in
      let phi2, axioms2, footprints2 = translate context psi2 domain in
      let fp_worklist =
        BatList.cartesian_product footprints1 footprints2
        |> List.filter (fun (fp1, fp2) -> SMT.Set.may_disjoint fp1 fp2)
      in

      let lst =
        fp_worklist
        |> List.map (fun (fp1, fp2) ->
            let phi1 = SMT.Term.substitute phi1 domain fp1 in
            let phi2 = SMT.Term.substitute phi2 domain fp2 in
            let disjoint = Set.mk_disjoint fp1 fp2 in
            let fp_union = Set.mk_union [fp1; fp2] context.fp_sort in
            let domain_def = Set.mk_eq domain fp_union in
            if SSL.has_unique_footprint psi1 && SSL.has_unique_footprint psi2 then
              Boolean.mk_and [phi1; phi2; disjoint; domain_def], Boolean.mk_true ()
            else
              let axioms, str_disjoint = mk_strongly_disjoint context fp1 fp2 in
              Boolean.mk_and [phi1; phi2; disjoint; str_disjoint; domain_def], axioms

          )
      in
      let semantics = Boolean.mk_or (List.map fst lst) in
      let axioms = Boolean.mk_and (List.map snd lst) in
      let axioms = Boolean.mk_and [axioms1; axioms2; axioms] in
      let footprints =
        fp_worklist
        |> List.map (fun (s1, s2) -> Set.mk_union [s1; s2] context.fp_sort)
      in
      (semantics, axioms, footprints)
    end

  and translate_guarded_neg context domain psi1 psi2 =
    let context' = {context with
      polarity = not context.polarity;
      can_scolemise = false
    }
    in
    let phi1, axioms1, footprints1 = translate context psi1 domain in
    let phi2, axioms2, footprints2 = translate context' psi2 domain in

    let phi2_neg = Boolean.mk_not phi2 in
    let semantics = Boolean.mk_and [phi1; phi2_neg] in
    let axioms = Boolean.mk_and [axioms1; axioms2] in
    (semantics, axioms, footprints1)

  and translate_septraction (context : Context.t) domain phi psi1 psi2 =
    let h1_name = Context.formula_witness_heap context phi in
    let h1 = Array.mk_var h1_name context.heap_sort in

    let fp1 = formula_footprint context psi1 in
    let fp2 = Set.mk_union [domain; fp1] context.fp_sort in

    let phi1, axioms1, footprints1 = translate {context with heap = h1} psi1 fp1 in
    let phi2, axioms2, footprints2 = translate {context with heap = h1} psi2 fp2 in

    let eq_fp = heaps_equal_on_footprint context context.heap h1 (Set.mk_diff fp2 fp1) in

    let disjoint = Set.mk_disjoint fp1 domain in
    let axioms = Boolean.mk_and [axioms1; axioms2] in
    let semantics = Boolean.mk_and [phi1; phi2; disjoint; eq_fp] in
    let footprints =
      BatList.cartesian_product footprints1 footprints2
      |> List.map (fun (s1, s2) -> Set.mk_diff s2 s1)
    in

    (* Positive septraction *)
    if context.can_scolemise then
      (semantics, axioms, footprints)

    (* Negative septraction *)
    else if SSL.has_unique_shape psi1 then
      let axiom, strongly_disjoint = mk_strongly_disjoint context fp1 domain in
      let semantics = Boolean.mk_and [semantics; strongly_disjoint] in
      let axioms = Boolean.mk_and [axioms; axiom] in
      (semantics, axioms, footprints)

    else failwith "Not supported form of septraction/magic wand"

let translate_phi context phi =
  let footprint = formula_footprint context phi in
  let phi, axioms, _ = translate context phi context.global_footprint in
  let nil = Var.mk "nil" context.locs_sort in
  let nil_not_in_fp = Boolean.mk_not (Set.mk_mem nil context.global_footprint) in

  let heap_nil = Array.mk_select context.heap nil in
  let heap_intro = Boolean.mk_eq nil heap_nil in

  let location_lemmas = Locations.location_lemmas context in

  Boolean.mk_and
              [phi; axioms; heap_intro; nil_not_in_fp; location_lemmas]

  (* ==== Translation of SMT model to stack-heap model ==== *)

  let translate_loc loc =
    try
      Term.show loc
      |> String.split_on_char '|'
      |> (fun xs -> List.nth xs 1)
      |> int_of_string
    with _ ->
      try int_of_string @@ Term.show loc
      with _ -> failwith ("Cannot convert location " ^ Term.show loc)

  let nil_interp context model =
    let e = Var.mk (SSL.Variable.show Nil) context.locs_sort in
    try Backend.eval model e
    with _ -> failwith "No interpretation of nil"

  let translate_stack context model =
    List.fold_left
      (fun stack var ->
        let var_expr = Var.mk (SSL.Variable.show var) context.locs_sort in
        let loc =
          try Backend.eval model var_expr
          with Not_found -> nil_interp context model
        in
        Stack.add var (translate_loc loc) stack
      ) Stack.empty context.vars

  let translate_heap context model heap_term fp =
    let fp =
      try SMT.Set.get_elems @@ Backend.eval model fp
      with Not_found -> failwith (Format.asprintf "Not found %s" (SMT.Term.show fp))
    in
    List.fold_left
      (fun heap loc ->
        let heap_image = Array.mk_select heap_term loc in
        let loc_name = translate_loc loc in
        let image_name =
          Backend.eval model heap_image
          |> translate_loc
        in
        Heap.add loc_name image_name heap
      ) Heap.empty fp

  let translate_footprints context model =
    SSL.fold
      (fun psi acc ->
        let id = SSL.subformula_id context.phi psi in
        let fp_name = Format.asprintf "footprint%d" id in
        let fp_expr = Var.mk fp_name context.fp_sort in
        let set = SMT.Set.get_elems @@ Backend.eval model fp_expr in
        let fp = Footprint.of_list @@ List.map translate_loc set in
        SSL.Map.add psi fp acc
      ) context.phi SSL.Map.empty

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
    (*let footprints = translate_footprints context model in*)
    let heaps = translate_witness_heaps context model in
    StackHeapModel.init ~heaps s h

  (* ==== Solver ==== *)

  let solve phi info =
    let context = init phi info in
    let phi = translate_phi context phi in
    let size = SMT.Term.size phi in

    Debug.context context;
    Print.debug "Translating
     - Stack bound: [%d, %d]
     - Location bound:  %d\n"
      (fst info.stack_bound)
      (snd info.stack_bound)
      info.heap_bound
     ;

    Print.debug "Running backend SMT solver\n";
    Timer.add "Astral";

    (* Solve *)
    Backend.init ();

    Debug.backend_translated (Backend.show_formula @@ Backend.translate phi);
    Debug.backend_simplified (Backend.show_formula @@ Backend.simplify @@ Backend.translate phi);

    match Backend.solve phi with
    | SMT_Sat model ->
      Debug.backend_model (Backend.show_model model);
      let sh = translate_model context model in
      Debug.model sh;
      let results = Results.create info (Some sh) size `SAT in
      Sat (StackHeapModel.empty (), results)

    | SMT_Unsat core ->
      let results = Results.create info None size `UNSAT in
      Unsat (results, core)

    | SMT_Unknown reason ->
      let results = Results.create info None size `UNKNOWN in
      Unknown (results, reason)
end
