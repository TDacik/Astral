open MemoryModel

module Logger = Debug.SubQueryDir (struct
  let dirname = "synthesis"
  let name = "Synthesis:popper-parser"
  let level = 3
end)


module Context = struct

  type t = {
    input : ParserContext_type.t;
    signature : Sort.t list;

    (* Output data *)
    root : SL.Term.t option;
    field_values : SL.Term.t MemoryModel.Field.Map.t;
    rest : SL.t list;
  }

  let init input signature = {
    input = input;
    signature = signature;
    root = None;
    field_values = MemoryModel.Field.Map.empty;
    rest = [];
  }

  (*let is_var ctx str = List.exists (fun v -> String.equal str @@ SL.Variable.get_name v) ctx.vars*)

  let add_root ctx root = {ctx with root = Some root}

  let add_field_value ctx field v =
    let fields = ParserContext.get_fields ctx.input in
    let field = List.find (fun f -> String.equal field @@ Field.get_name f) fields in
    {ctx with field_values = MemoryModel.Field.Map.add field v ctx.field_values}

  let add_atom ctx atom = {ctx with rest = atom :: ctx.rest}

  let gen_body ctx =
    match ctx.root with
    | None -> SL.mk_star ctx.rest
    | Some root ->
      let sort = SL.Term.get_sort root in
      let struct_def = HeapSort.find_target sort ctx.input.heap_sort in
      let fields = StructDef.get_fields struct_def in
      let values = List.map (fun f ->
        try MemoryModel.Field.Map.find f ctx.field_values
        with Not_found -> failwith @@ Format.sprintf "Fail for field %s" (Field.show f)
      ) fields in
      let pto = SL.mk_pto_struct root struct_def values in
      (* TODO: existential closure *)
      SL.mk_star (pto :: ctx.rest)

end

open Context

type solution = {
  precision : float;
  recall : float;
  predicates : InductiveDefinition.t list;
}

(* TODO: correct typing *)
let parse_var ctx name =
  let n = int_of_string @@ BatString.lchop name in
  let sort =
    if n < List.length ctx.signature
    then List.nth ctx.signature n
    else Sort.loc_nil (* TODO *)
  in
  SL.Term.mk_var name sort

let parse_call header =
  Logger.debug "Parsing call: %s\n" header;
  let call = BatString.rchop @@ BatString.trim header in
  match String.split_on_char '(' call with
  | [name; params] -> name, String.split_on_char ',' params
  | _ -> failwith header

let parse_atom ctx atom =
  let name, vars = parse_call atom in
  match name with
  | "eq" -> Context.add_atom ctx @@ SL.mk_eq @@ List.map (parse_var ctx) vars
  | "neq" -> Context.add_atom ctx @@ SL.mk_distinct @@ List.map (parse_var ctx) vars
  | "remove" -> Context.add_root ctx @@ (parse_var ctx) @@ List.nth vars 1
  | field when ParserContext.is_declared_field ctx.input field ->
    Logger.debug "Adding target %s for %s\n" (List.nth vars 1) field;
    Context.add_field_value ctx field @@ (parse_var ctx) @@ List.nth vars 1
  | pred when String.equal pred "pred" || ParserContext.is_declared_pred ctx.input pred ->
    let params = BatList.take (List.length vars - 1) vars in
    Context.add_atom ctx @@ SL.mk_predicate name @@ List.map (parse_var ctx) params
  | atom ->
    Logger.debug "Ignoring %s during conversion\n" atom;
    ctx

let parse_body ctx params body =
  Logger.debug "Parsing body: %s\n" body;
  BatString.split_on_string ~by:")," body
  |> List.map (fun s -> if not @@ String.ends_with ~suffix:")" s then s ^ ")" else s)
  |> List.fold_left parse_atom ctx
  |> Context.gen_body

let parse_predicate ctx line =
  Logger.debug "Parsing predicate clause: %s\n" line;
  (* Remove dot at the end. *)
  let line = String.sub (BatString.trim line) 0 (String.length line - 1) in
  match BatString.split_on_string ~by:":-" line with
  | [header; body] ->
    let name, params = parse_call header in
    let sort = Sort.loc_nil in (* TODO *)
    let params = List.map (fun name -> SL.Variable.mk name sort) params in
    let body = parse_body ctx params body in
    let params = BatList.take (List.length params - 1) params in
    (name, params, body)
  | _ -> assert false

let group_by_name defs =
  List.fold_right (fun (name, params, def) acc ->
    match List.assoc_opt name acc with
    | Some (_, defs) -> (name, (params, def :: defs)) :: List.remove_assoc name acc
    | None -> (name, (params, [def])) :: acc
  ) defs []
  |> List.map (fun (name, (params, defs)) -> InductiveDefinition.mk name params (SL.mk_or defs))

let parse_predicates ctx lines =
  List.filter (fun s -> s <> "") lines
  |> List.map (parse_predicate ctx)
  |> group_by_name

let parse_stats stats =
  Scanf.sscanf stats "Precision:%f Recall:%f" (fun precision recall -> precision, recall)

let convert input signature popper_out =
  let re = Str.regexp "\\*+ SOLUTION \\*+[\n]+\\([^*]+\\)[\n]*\\*+" in
  let _ = Str.search_forward re popper_out 0 in
  let solution = Str.matched_group 1 popper_out in
  let lines = String.split_on_char '\n' solution in
  let precision, recall = parse_stats @@ List.hd lines in
  let ctx = Context.init input signature in
  let predicates = parse_predicates ctx @@ List.tl lines in
  {precision; recall; predicates}
