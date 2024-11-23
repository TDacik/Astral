(*
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Location_sig

module Logger = Logger.Make(struct let name = "LocationBuilder" let level = 2 end)

module Make (Locations : LOCATIONS_BASE) = struct

  include Locations

  let show self =
    let show_val (term, consts) =
      Format.asprintf "(%s, {%s})"
        (SMT.show term)
        (String.concat ", " @@ List.map SMT.show consts)
    in
    Format.asprintf "  Internal encoding: %s\n  Sort encoding: %s\n  Mappers: %s"
      (Locations.show self)
      (Sort.Map.show show_val self.sort_encoding)
      (Sort.Map.show (fun _ -> "...") self.mapping)

  (** Accessors *)

  let null locs = (*match Sort.Map.choose_opt locs.mapping with
    | Some (_, mapper) -> SMT.Array.mk_select mapper locs.null
    | None ->*) locs.null

  let internal_error self msg_prefix =
    let msg = Format.asprintf "%s in location encoding:\n%s" msg_prefix (show self)
    in Utils.internal_error msg

  let get_encoding locs sort =
    try Sort.Map.find sort locs.sort_encoding
    with Not_found -> internal_error locs ("Sort encoding of " ^ Sort.show sort ^ " not found")

  let get_mapper locs sort =
    try Sort.Map.find sort locs.mapping
    with Not_found -> internal_error locs ("Mapping of sort " ^ Sort.show sort ^ " not found")

  let sort_encoding locs sort = fst @@ get_encoding locs sort

  let sort_constants locs sort = snd @@ get_encoding locs sort

  let get_to_loc locs sort = fst @@ get_mapper locs sort

  let get_of_loc locs sort = snd @@ get_mapper locs sort

  (** Auxiliary definitions *)

  let set_sort locs = SMT.Sets.mk_sort locs.sort

  (** Declaration *)

  let mk_var locs name = SMT.mk_var name locs.sort

  let mk_fresh_var locs name = SMT.mk_fresh_var name locs.sort

  let mk_set_var locs name = SMT.mk_var name (set_sort locs)

  let mk_fresh_set_var locs name = SMT.mk_fresh_var name (set_sort locs)


  (** Translation *)

  let translate_sort locs _ = locs.sort

  let translate_var locs var = SMT.Variable.mk (SL.Variable.show var) locs.sort

  let rec translate_term locs term = match SL.Term.view term with
    | SL.Term.Var var -> SMT.mk_var (SL.Variable.show var) locs.sort
    | SL.Term.SmtTerm t ->
      let sort = SMT.get_sort t in
      let mapper = get_to_loc locs sort in
      SMT.Array.mk_select mapper t
    | SL.Term.BlockBegin t ->
      let sort = Sort.mk_bitvector 2 in
      let arr = SMT.Array.mk_var "block_begin" @@ Sort.mk_array locs.sort locs.sort in
      let to_loc = get_to_loc locs sort in
      let from_loc = get_of_loc locs sort in
      SMT.Array.mk_select arr
      @@ translate_term locs t

  (** Typing *)

  let mk_of_type locs var sl_sort = SMT.Sets.mk_mem var (sort_encoding locs sl_sort)

  let mk_set_of_type locs set sl_sort =
    try SMT.Sets.mk_subset set (sort_encoding locs sl_sort)
    with Not_found -> SMT.Sets.mk_eq_empty set (* TODO: should not happen *)

  (** Quantifiers *)

  let quantify locs quantifier n expr_constructor =
    let binders = List.init n (fun _ -> SMT.Variable.mk_fresh "loc" locs.sort) in
    let expr = expr_constructor (List.map SMT.of_var binders) in
    match quantifier with
    | `Forall -> SMT.Quantifier.mk_forall binders expr
    | `Exists -> SMT.Quantifier.mk_exists binders expr

  let mk_forall locs constructor = quantify locs `Forall 1 (fun [x] -> constructor x)
  let mk_exists locs constructor = quantify locs `Exists 1 (fun [x] -> constructor x)

  let mk_forall' locs n constructor = quantify locs `Forall n constructor
  let mk_exists' locs n constructor = quantify locs `Exists n constructor


  (** {2 Inverse translation} *)

  let get_index self loc = match List_utils.index_of_cmp SMT.compare loc self.constants with
    | Some index -> index
    | None -> internal_error self (Format.sprintf "Index of location %s" (SMT.show loc))

  let inverse_translate_sort self loc =
    Sort.Map.bindings self.sort_encoding
    |> List.find (fun (_, (_, locs)) -> BatList.mem_cmp SMT.compare loc locs)
    |> fst


  let inverse_translate_aux self model loc =
    let index = get_index self loc in
    if index = 0 then StackHeapModel.Location.mk_nil index
    else
      let sort = inverse_translate_sort self loc in
      let name = SMT.show loc in
      StackHeapModel.Location.mk index sort

  let inverse_translate self model loc_const =
    let sort = Constant.get_sort loc_const in
    assert (Sort.equal sort self.sort);
    let res =
      (* Translate using index in sort encoding *)
      if Sort.Map.is_empty self.mapping then
        inverse_translate_aux self model (SMT.of_const loc_const)
      (* Translate using interpretation of mapper *)
      else
        let orig_sort = inverse_translate_sort self (SMT.of_const loc_const) in
        let mapper = get_of_loc self orig_sort in
        let interp = SMT.Model.eval model mapper in
        let const = Constant.select interp loc_const in
        StackHeapModel.Location.mk_smt const
    in
    Logger.debug "Constant [|%s|] = %s\n"
      (Constant.show loc_const)
      (StackHeapModel.Location.show res);
    res

  (** Axioms *)

  let term_axiom locs term =
    if SL.Term.is_heap_term term then SMT.Boolean.tt
    else if SL.Term.is_nil term then
      let term' = translate_term locs term in
      SMT.mk_eq [locs.null; term']
    else
      let sort = SL.Term.get_sort term in
      let sort_set = sort_encoding locs sort in
      let term' = translate_term locs term in
      mk_of_type locs term' sort

  let sort_axiom locs sort =
    let set = sort_encoding locs sort in
    let consts = sort_constants locs sort in
    let set_def = SMT.Sets.mk_constant (set_sort locs) consts in
    SMT.Sets.mk_eq [set; set_def]

  let convert locs term = match SL.Term.view term with
    | SL.Term.Var var -> SMT.mk_var (SL.Variable.show var) (SL.Variable.get_sort var)
    | SL.Term.SmtTerm t -> t
    | SL.Term.BlockBegin t -> failwith "TODO"

  let loc_mapping_axioms self phi =
    let axiom1 = Sort.Map.fold (fun sort (to_loc, from_loc) acc ->
      mk_forall self (fun loc ->
        let term = SMT.Array.mk_select to_loc @@ SMT.Array.mk_select from_loc loc in
        SMT.Boolean.mk_eq [term; loc]
      )
    ) self.mapping SMT.Boolean.tt
    in
    let axiom2 = Sort.Map.fold (fun sort (to_loc, from_loc) acc ->
      SL.get_terms_of_sort sort phi
      |> List.map (convert self)
      |> List.map (fun t ->
        let t' = SMT.Array.mk_select from_loc @@ SMT.Array.mk_select to_loc t in
        SMT.Boolean.mk_eq [t; t']
      )
      |> SMT.Boolean.mk_and
      |> SMT.Boolean.mk_and2 acc
    ) self.mapping SMT.Boolean.tt
    in
    SMT.Boolean.mk_and [axiom1; axiom2]

  let axioms locs phi =
    let terms = SL.Term.nil :: SL.get_loc_terms phi locs.heap_sort in
    let sorts = HeapSort.get_loc_sorts locs.heap_sort in
    let term_axioms = SMT.Boolean.mk_and @@ List.map (term_axiom locs) terms in
    let sort_axioms = SMT.Boolean.mk_and @@ List.map (sort_axiom locs) sorts in
    let mapping_axioms = loc_mapping_axioms locs phi in
    SMT.Boolean.mk_and [term_axioms; sort_axioms; mapping_axioms]

  (** Utilities *)

  let rec powerset = function
    | [] -> [[]]
    | x :: xs ->
      let ps = powerset xs in
      ps @ List.map (fun s -> x :: s) ps

  let powerset locs = powerset locs.constants

end
