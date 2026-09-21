open ParserContext_type

module Logger = Debug.SubQueryDir (struct
  let dirname = "synthesis"
  let name = "Synthesis"
  let level = 1
end)

module ID = Identifier.Make()

let get_example ctx phi =
  let res = Engine.solve {ctx with assertions = [phi]; produce_models = true} in
  match res.model with
  | Some model -> model
  |  _ -> failwith "No model"

let common_and_rest (lists : 'a list list) : 'a list * 'a list list =
  match lists with
  | [] -> ([], [])
  | first :: _ ->
    let common =
      List.filter
        (fun x -> List.for_all (fun l -> SL.MonoList.mem x l) lists)
        first
    in
    let rest =
      List.map
        (fun l -> List.filter (fun x -> not (SL.MonoList.mem x common)) l)
        lists
    in
    (common, rest)

(* TODO: think more about what is relevant... *)
let get_relevant_vars phi =
  let qf, atoms = SL.as_quantified_symbolic_heap phi in
  let atoms = List.filter (fun a -> not @@ SL.is_distinct a) atoms in
  let phi = SL.mk_exists qf @@ SL.mk_star atoms in
  SL.free_vars ~with_nil:false phi
  |> SL.Variable.MonoList.unique

(*
let reduce_models models =
  let decompositions = List.map StackHeapModel.chunk_decomposition models in
*)

let split3 (l : ('a * 'b * 'c) list) : 'a list * 'b list * 'c list =
  List.fold_right
    (fun (x, y, z) (xs, ys, zs) -> (x :: xs, y :: ys, z :: zs))
    l
    ([], [], [])

let qf_atoms phi =
  let qf, atoms = SL.as_quantified_symbolic_heap phi in
  let x, y = List.partition (SL.is_ground' ~forbidden:qf) atoms in
  (qf, x, y)

let reduce_examples formulae =
  let qfs, candidate_atoms, rest_atoms = split3 @@ List.map qf_atoms formulae in
  let frame, rest = common_and_rest candidate_atoms in
  frame, List.mapi (fun i es -> SL.mk_exists es @@ SL.mk_star (List.nth rest i @ List.nth rest_atoms i)) qfs

let compute_abstraction_aux ctx =
  Logger.debug "Starting synthesis\n";
  Logger.debug "Positive examples:\n";
  List.iter (fun phi -> Logger.debug "  %s\n" (SL.show phi)) ctx.positive_examples;
  Logger.debug "Negative examples:\n";
  List.iter (fun phi -> Logger.debug "  %s\n" (SL.show phi)) ctx.negative_examples;

  let frame, pexamples_ = reduce_examples ctx.positive_examples in
  Logger.debug "Frame: %s\n" (SL.show @@ SL.mk_star frame);
  Logger.debug "Reduced examples:\n";
  List.iter (fun phi -> Logger.debug "  %s\n" (SL.show phi)) pexamples_;

  let pexamples = List.map (get_example ctx) pexamples_ in

  List.iteri (fun i sh -> Logger.sl_formula (Format.asprintf "positive_%d" i) sh) pexamples_;
  List.iteri (fun i sh -> Logger.sl_model (Format.asprintf "positive_%d" i) sh) pexamples;

  Logger.debug "Converting models to prolog\n";

  let vars = get_relevant_vars @@ SL.mk_star pexamples_ in
  SL.Variable.print_list ~prefix:"Vars for synthesis" vars;

  let name = ID.show @@ ID.mk_fresh "pred" in
  let name = BatString.nreplace ~str:name ~sub:"!" ~by:"" in

  let res = List.fold_left2 (PrologConvertor.convert name vars) PrologConvertor.Result.empty pexamples ctx.positive_examples in

  Logger.debug "Generating Popper files\n";

  let fields = SL.get_fields @@ List.hd ctx.positive_examples in
  let knowledge = PrologConvertor.Result.generate_knowledge (GlobalSID.get_user_defined ~original:true ()) res fields in
  let examples = PrologConvertor.Result.generate_examples res in

  let arity = List.length vars in
  let signature = List.map SL.Variable.get_sort vars in (* TODO: what is the correct order? *)
  let predicates = (name, arity) :: (GlobalSID.get_signatures () |> List.map (fun (p, xs) -> p, List.length xs)) in
  let bias = BiasGenerator.generate predicates name arity fields in

  Logger.debug "Running Popper\n";

  let ok, response = PopperDriver.synthesize ~bias ~knowledge ~examples () in
  if not ok
  then Exceptions.internal_error ~reason:"Popper" ~details:response
  else
    let res = SolutionConvertor.convert ctx vars signature response in
    match res with
      | None -> Format.printf "unsat"; None
      | Some res ->
      (
        (if res.precision <> 1.0 || res.recall <> 1.0 then
          ReportUtils.warning "Synthesis result is only approximate (precision: %0.1f, recall: %0.1f)\n"
            res.precision
            res.recall
        );

      List.iter (fun def -> Format.printf "%s\n" (InductiveDefinition.smt2_decl def)) res.predicates;

      Some (frame, res)
      )

let compute_abstraction ctx =
  let _ = compute_abstraction_aux ctx in
  exit 0

let compute_abstraction' ctx =
  compute_abstraction_aux ctx
