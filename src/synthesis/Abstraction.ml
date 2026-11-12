open ParserContext_type

module Logger = Logging.WithOutput (struct
  let name = "Abstraction solver"
  let level = 1
  let path = `Query ""
end)

(* TODO: move to this to a separated API module *)



let context (solver : Solver.solver) examples =
  let vars =
    List.concat_map (SL.free_vars ~with_nil:false) examples
    |> SL.Variable.MonoList.unique
  in
  let heap_sort = Solver.get_heap_sort solver in
  let input =
    let input = ParserContext.empty () in
    let heap_sort = HeapSort.to_list input.heap_sort @ HeapSort.to_list heap_sort in
    let input = ParserContext.declare_heap_sort input heap_sort in
    ParserContext.add_vars input vars
  in
  let structs = HeapSort.get_structures heap_sort in
  let input =
    List.fold_left (fun input def ->
      let module M = Map.Make(String) in
      let cons = MemoryModel.StructDef.get_constructor def in
      {input with struct_defs = M.add cons def input.struct_defs}
    ) input structs
  in
  {input with positive_examples = examples}

(*
let compute (solver : Solver.solver) examples =
  let ctx = context solver examples in
  match Synthesiser.compute_abstraction' ctx with
  | Some (frame, solution) when SolutionConvertor.is_precise solution ->
    let predicate = List.hd solution.predicates in
    let name = InductiveDefinition.name predicate in
    let args = List.map SL.Term.of_var solution.concrete_vars in
    let phi = SL.mk_star (SL.mk_predicate name args :: frame) in
    Some (phi, [predicate])
  | _ -> None
*)

(** High-level interface *)

let compute_chunk ctx (root, positive_examples : SL.Term.t * SL.t list) =
  let open SolutionConvertor in
  Logger.debug "Computing query for root %a:\n" SL.Term.pp root;
  let ctx = {ctx with positive_examples} in
  match PredicateMapping.mmatch ctx with
  | Some res ->
    Logger.debug " -- Success: %a:\n" SL.pp res;
    Some (res, [])
  | None ->
    Logger.debug " -- Needs synthesis\n";
    match Synthesiser.compute_abstraction' ctx with
    | None -> None
    | Some (_, res) ->
      assert (List.length res.predicates == 1);
      let pred = List.hd res.predicates in
      let params = List.map SL.Term.of_var res.concrete_vars in
      GlobalSID.register_user_defined pred;
      match PredicateMapping.check_instance ctx pred params with
      | Some phi -> Some (phi, [pred])
      | None ->
        Exceptions.internal_error "Cannot map learned predicate on the input:\n%a"
          InductiveDefinition.pp pred

let concat_results acc res = match acc, res with
  | _, None -> None
  | None, Some (phi, preds) -> Some ([phi], preds)
  | Some (phi_acc, preds_acc), Some (phi, preds) -> Some (phi :: phi_acc, preds_acc @ preds)

let compute_aux ctx =
  let vars = ParserContext.get_sl_vars ctx in
  let examples = ctx.positive_examples in

  let frame, chunk_classes = Spliter.split examples vars in
  SL.print_list ~prefix:"Frame:" frame;
  let res = List.map (compute_chunk ctx) chunk_classes in
  match List.fold_left concat_results None res with
  | None -> None
  | Some (phis, preds) ->
    let phi = SL.mk_star (frame @ phis) in
    Logger.output "result.smt2" (BenchmarkOutput.out_res ctx) phi;
    Some (phi, preds)

let compute solver examples =
  LoggerState.new_query `Abstraction;
  let ctx = context solver examples in
  Logger.output "input.smt2" (BenchmarkOutput.out ctx) examples;
  compute_aux ctx

let compute_and_print ctx =
  match compute_aux ctx with
  | None ->
    Format.printf "Abstraction failed";
    exit 1
  | Some (phi, preds) ->
    List.iter (fun def -> Format.printf "%s\n" (InductiveDefinition.smt2_decl def)) preds;
    Format.printf "%s\n" (SL.to_smt2 phi);
    exit 0 (* should return? *)
