(* Internal context of synthesis.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

module Logger = Logging.WithOutput (struct
  let name = "Synthesis"
  let level = 1
  let path = `Toplevel ""
end)

(** Identifiers for predicates to be learned. *)
module ID = Identifier.Make()

type t = {
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
    (MemoryModel.Field.show_list ctx.fields)
    (SL.Variable.show_list ctx.vars)

let root ctx = List.hd ctx.vars

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

let compute_signature vars models =
  let root = match List.filter_map get_unique_root models with
    | [] -> assert false
    | [[x]] -> x
    | [_] -> assert false
    | xs :: rest ->
      List.find (fun x -> List.exists (fun ys -> SL.Variable.MonoList.mem x ys) rest) xs
  in
  root :: SL.Variable.MonoList.remove vars root

let init pos_examples neg_examples pos_models neg_models =
  let examples = pos_examples @ neg_examples in
  let vars_aux = get_relevant_vars @@ SL.mk_star examples in
  let fields =
    List.concat_map SL.get_fields examples
    |> MemoryModel.Field.MonoList.unique
  in
  let predicates = GlobalSID.get_user_defined ~original:true () in
  let vars = compute_signature vars_aux pos_models in
  let pred = ID.show @@ ID.mk_fresh "pred" in
  let pred = BatString.nreplace ~str:pred ~sub:"!" ~by:"" in
  {fields; predicates; pos_examples; neg_examples; pos_models; neg_models; pred; vars}
