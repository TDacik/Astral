open ParserContext_type

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
