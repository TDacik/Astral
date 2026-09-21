open MemoryModel

module Logger = Debug.SubQueryDir (struct
  let dirname = "synthesis"
  let name = "Synthesis:popper-parser"
  let level = 3
end)

module HT = Hashtbl.Make(Int)

let existentials : Sort.t HT.t ref = ref @@ HT.create 113

module Context = struct

  type t = {
    input : ParserContext_type.t;
    signature : Sort.t list;

    (* Output data *)
    root : SL.Term.t;
    field_values : SL.Term.t MemoryModel.Field.Map.t;
    rest : SL.t list;
  }

  let init input signature = {
    input = input;
    signature = signature;
    root = SL.Term.mk_var "V0" (List.hd signature);
    field_values = MemoryModel.Field.Map.empty;
    rest = [];
  }

  (*let is_var ctx str = List.exists (fun v -> String.equal str @@ SL.Variable.get_name v) ctx.vars*)

  let add_field_value ctx field (v : string) =
    let fields = ParserContext.get_fields ctx.input in
    let field = List.find (fun f -> String.equal field @@ Field.get_name f) fields in

    let n = int_of_string @@ BatString.lchop v in
    let sort = Field.get_sort field in
    (if n >= List.length ctx.signature && not @@ String.equal v "nil" then
      HT.add !existentials n sort);

    let v = if String.equal v "nil" then SL.Term.nil else SL.Term.mk_var v sort in

    {ctx with field_values = MemoryModel.Field.Map.add field v ctx.field_values}

  let add_atom ctx atom = {ctx with rest = atom :: ctx.rest}

  let gen_body ctx =
    match ctx.field_values with
    | _ when MemoryModel.Field.Map.is_empty ctx.field_values -> SL.mk_star ctx.rest
    | _ ->
      let sort = SL.Term.get_sort ctx.root in
      let struct_def = HeapSort.find_target sort ctx.input.heap_sort in
      let fields = StructDef.get_fields struct_def in
      let values = List.map (fun f ->
        try MemoryModel.Field.Map.find f ctx.field_values
        with Not_found -> failwith @@ Format.sprintf "Fail for field %s" (Field.show f)
      ) fields in
      let pto = SL.mk_pto_struct ctx.root struct_def values in
      let es =
        HT.fold
          (fun n sort acc -> (SL.Variable.mk (Format.asprintf "V%d" n) sort) :: acc)
          !existentials []
      in
      SL.Variable.print_list es;
      SL.mk_exists es @@ SL.mk_star (pto :: ctx.rest)

end

open Context

type solution = {
  precision : float;
  recall : float;
  concrete_vars : SL.Variable.t list;
  predicates : InductiveDefinition.t list;
}

let is_precise solution = not (solution.precision <> 1.0 || solution.recall <> 1.0)

let check_types what terms =
  let sorts = List.map SL.Term.get_sort terms in
  if List_utils.all_equal SL.Sort.equal sorts then true
  else (
    Logger.warning "Silently ignoring incorrectly typed '%s(%s)'"
      what
      (SL.Term.show_list terms)
    ;
    false
  )

let parse_var ctx name =
  let n = int_of_string @@ BatString.lchop name in
  let sort =
    if n < List.length ctx.signature
    then List.nth ctx.signature n
    else let _ = Format.printf "%d\n" n in HT.find !existentials n
  in
  SL.Term.mk_var name sort

let parse_call header =
  Logger.debug "Parsing call: %s\n" header;
  let call = BatString.rchop @@ BatString.trim header in
  match String.split_on_char '(' call with
  | [name; params] -> name, String.split_on_char ',' params
  | _ -> failwith header

let is_field_nil ctx str =
  match String.split_on_char '_' str with
  | [field; "is"; "nil"] -> ParserContext.is_declared_field ctx.input field
  | _ -> false

let parse_atom ctx atom =
  let name, vars = parse_call atom in
  match name with
  | "eq" ->
    let vars = List.map (parse_var ctx) vars in
    if check_types "eq" vars
    then Context.add_atom ctx @@ SL.mk_eq vars
    else ctx
  | "neq" ->
    let vars = List.map (parse_var ctx) vars in
    if check_types "neq" vars
    then Context.add_atom ctx @@ SL.mk_distinct vars
    else ctx
  | field when ParserContext.is_declared_field ctx.input field ->
    let target = List.nth vars 1 in
    Logger.debug "Adding target %s for %s\n" target field;
    Context.add_field_value ctx field target
  | field when is_field_nil ctx field ->
    let field = List.hd @@ String.split_on_char '_' field in
    Logger.debug "Adding target nil for %s\n" field;
    Context.add_field_value ctx field "nil"

  | pred when String.starts_with pred ~prefix:"pred" || ParserContext.is_declared_pred ctx.input pred ->
    (*let params = BatList.take (List.length vars - 1) vars in*)
    Context.add_atom ctx @@ SL.mk_predicate name @@ List.map (parse_var ctx) vars
  | atom ->
    Logger.debug "Ignoring %s during conversion\n" atom;
    ctx

let parse_body ctx params body =
  Logger.debug "Parsing body: %s\n" body;
  let weight s =
    if ParserContext.is_declared_field ctx.input s || is_field_nil ctx s then 1
    else 0
  in
  let aux_cmp s1 s2 = Int.compare (weight s1) (weight s2) in
  BatString.split_on_string ~by:")," body
  |> List.map (fun s -> if not @@ String.ends_with ~suffix:")" s then s ^ ")" else s)
  |> List.sort aux_cmp
  |> List.fold_left parse_atom ctx
  |> Context.gen_body

let parse_predicate ctx line =
  Logger.debug "Parsing predicate clause: %s\n" line;
  (* Remove dot at the end. *)
  let line = String.sub (BatString.trim line) 0 (String.length line - 1) in
  match BatString.split_on_string ~by:":-" line with
  | [header; body] ->
    let name, params = parse_call header in
    let params = List.mapi (fun i name -> SL.Variable.mk name @@ List.nth ctx.signature i) params in
    let body = parse_body ctx params body in
    (*let params = BatList.take (List.length params - 1) params in*)
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

let convert input concrete_vars signature popper_out =
  if BatString.exists popper_out "NO SOLUTION" then None
  else
    let _ = HT.clear !existentials in
    let re = Str.regexp "\\*+ SOLUTION \\*+[\n]+\\([^*]+\\)[\n]*\\*+" in
    let _ = Str.search_forward re popper_out 0 in
    let solution = Str.matched_group 1 popper_out in
    let lines = String.split_on_char '\n' solution in
    let precision, recall = parse_stats @@ List.hd lines in
    let ctx = Context.init input signature in
    let predicates = parse_predicates ctx @@ List.tl lines in
    Some {precision; recall; concrete_vars; predicates}
