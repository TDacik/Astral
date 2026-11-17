(* Internal context of synthesis.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

module Logger = Logging.WithOutput (struct
  let name = "Synthesis"
  let level = 1
  let path = `Toplevel ""
end)

type t = {
  heap_sort : HeapSort.t;

  (* Existing definitions *)
  fields : MemoryModel.Field.t list;
  predicates : InductiveDefinition.t list;

  (* Learning input *)
  pos_examples : SL.t list;
  neg_examples : SL.t list;
  pos_models : StackHeapModel.t list;
  neg_models : StackHeapModel.t list;

  (* Instantiation to be learned *)
  pred : string;
  vars : SL.Variable.t list;
}

(** API *)

let show ctx =
  Format.asprintf "Context:\n  fields: %s\n  vars: %s\n"
    (String.concat ", " @@ List.map MemoryModel.Field.show_with_sort ctx.fields)
    (SL.Variable.show_list ctx.vars)

let root ctx = List.hd ctx.vars

let root_fields ctx =
  root ctx
  |> SL.Variable.get_sort
  |> (fun s -> HeapSort.find_target s ctx.heap_sort)
  |> MemoryModel.StructDef.get_fields

(* TODO: think more about what is relevant... *)
let get_relevant_vars phi =
  let qf, atoms = SL.as_quantified_symbolic_heap phi in
  let atoms = List.filter (fun a -> not @@ SL.is_distinct a) atoms in
  let phi = SL.mk_exists qf @@ SL.mk_star atoms in
  SL.free_vars ~with_nil:false phi
  |> SL.Variable.MonoList.unique

let get_unique_root sh =
  match StackHeapModel.get_roots sh with
  | [] -> None
  | [x] -> Some (List.map SL.Term.as_var @@ StackHeapModel.stack_inverse sh x)
  | _ ->
    Logger.sl_model "multiple_roots" sh;
    failwith "multiple roots"

let next_name =
  let cnt = ref 1 in
  fun () ->
    while GlobalSID.mem (Format.asprintf "pred%d" !cnt) do
      incr cnt
    done;
    Format.asprintf "pred%d" !cnt

let init ctx pos_examples neg_examples pos_models neg_models vars =
  let examples = pos_examples @ neg_examples in
  let vars_aux = get_relevant_vars @@ SL.mk_star examples in
  let fields = ParserContext.get_fields ctx in
  let heap_sort = ParserContext.get_heap_sort ctx in
  let predicates = GlobalSID.get_user_defined ~original:true () in
  let pred = next_name () in
  let pred = BatString.nreplace ~str:pred ~sub:"!" ~by:"" in
  {heap_sort; fields; predicates; pos_examples; neg_examples; pos_models; neg_models; pred; vars}
