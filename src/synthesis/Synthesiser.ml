open ParserContext_type

module Logger = Logging.WithOutput (struct
  let name = "Synthesis"
  let level = 1
  let path = `CurrentSubquery "synthesis"
end)

module ID = Identifier.Make()

let get_model ctx phi =
  let res = Engine.solve {ctx with assertions = [phi]; produce_models = true} in
  match res.model with
  | Some model -> model
  |  _ -> failwith @@ Format.asprintf "No model: %s" (SL.show phi)

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

let compute ctx root positive_examples =
  Logger.debug "Starting synthesis for root %s\n" (SL.Variable.show_list root);
  let positive_models = List.map (get_model ctx) positive_examples in
  let synth_ctx = SynthesisContext.init ctx positive_examples [] positive_models [] root in
  Logger.debug "%s" (SynthesisContext.show synth_ctx);

  (* Generate popper files *)
  let knowledge, examples = PrologConvertor.convert synth_ctx in
  let bias = BiasGenerator.generate synth_ctx in

  (* Run popper *)
  let ok, response = PopperDriver.synthesize ~bias ~knowledge ~examples () in
  if not ok
  then Exceptions.internal_error "Synthesis failed" ~details:response
  else
    let res =
      SolutionConvertor.convert ctx synth_ctx response
    in
    match res with
      | None -> None
      | Some res -> (
        let res = SynthesisResult.canonic_var_names res in
        Logger.debug "%s\n" (InductiveDefinition.smt2_decl @@ List.hd res.predicates);

        (if not @@ SynthesisResult.is_precise res then
          ReportUtils.warning "Synthesis result is only approximate (precision: %0.1f, recall: %0.1f)\n"
            res.precision
            res.recall
        );
        Some res)

let compute ctx root examples =
  LoggerState.new_subquery `Synthesis;
  let res = compute ctx root examples in
  LoggerState.finish_subquery ();
  res
