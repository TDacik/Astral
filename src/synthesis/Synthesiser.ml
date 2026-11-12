open ParserContext_type

module Logger = Logging.WithOutput (struct
  let name = "Synthesis"
  let level = 1
  let path = `CurrentSubquery "synthesis"
end)

module ID = Identifier.Make()

let get_example ctx phi =
  SL.print ~prefix:"Target" phi;
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

  let synth_ctx = SynthesisContext.init ctx.positive_examples ctx.negative_examples pexamples [] in
  Logger.debug "%s" (SynthesisContext.show synth_ctx);

  (* Generate popper files *)
  let knowledge, examples = PrologConvertor.convert synth_ctx in
  let bias = BiasGenerator.generate synth_ctx in

  (* Run popper *)
  let ok, response = PopperDriver.synthesize ~bias ~knowledge ~examples () in
  if not ok
  then Exceptions.internal_error "Popper failed" ~details:response
  else
    let res = SolutionConvertor.convert ctx synth_ctx response in
    match res with
      | None -> None
      | Some res -> (
        (if not @@ SynthesisResult.is_precise res then
          ReportUtils.warning "Synthesis result is only approximate (precision: %0.1f, recall: %0.1f)\n"
            res.precision
            res.recall
        );
        Some (frame, res))


  (*

  let arity = List.length vars in
  let signature = List.map SL.Variable.get_sort vars in (* TODO: what is the correct order? *)
  let predicates = (name, arity) :: (GlobalSID.get_signatures () |> List.map (fun (p, xs) -> p, List.length xs)) in
  let bias = BiasGenerator.generate predicates name arity fields in

  Logger.debug "Running Popper\n";
  *)

let compute_abstraction ctx =
  let _ = compute_abstraction_aux ctx in
  exit 0

let compute_abstraction' ctx =
  compute_abstraction_aux ctx
