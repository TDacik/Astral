open MemoryModel
open SynthesisContext

module Logger = Logging.WithOutput (struct
  let name = "Synthesis:popper-parser"
  let level = 3
  let path = `Query "popper-parser"
end)

module HT = struct
  include Hashtbl.Make(Int)

  let find ht var =
    try find ht var
    with Not_found ->
      Format.printf "Hashtable:\n";
      iter (fun id sort -> Format.printf " %d -> %s\n" id (Sort.show sort)) ht;
      failwith @@ Format.asprintf "Cannot find %d" var

end


let existentials : Sort.t HT.t ref = ref @@ HT.create 113

module Result = struct

  type t = {
    input : ParserContext_type.t;
    signature : Sort.t list;

    (* Output data *)
    root : SL.Term.t;
    field_values : SL.Term.t MemoryModel.Field.Map.t;
    rest : SL.t list;
  }

  let init input ctx =
    let signature = List.map SL.Variable.get_sort ctx.vars in
    {
      input = input;
      signature = signature;
      root = SL.Term.mk_var "V0" @@ List.hd signature;
      field_values = MemoryModel.Field.Map.empty;
      rest = [];
    }

  let add_field_value ctx field (v : string) =
    let fields = ParserContext.get_fields ctx.input in
    Format.printf "Looking for: %s\n" field;
    Format.printf "In: %s\n" (String.concat ", " @@ List.map PopperEncoding.field fields);
    let field = List.find
      (fun f -> String.equal (Field.show f) field) fields in

    let v =
      if String.equal v "nil" then SL.Term.nil
      else
        let n = int_of_string @@ BatString.lchop v in
        let sort = Field.get_sort field in
        (if n >= List.length ctx.signature && not @@ String.equal v "nil" then
          HT.add !existentials n sort);
        SL.Term.mk_var v sort
    in

    Format.printf "Adding term %s learned from field %s" (SL.Term.show_with_sort v) (Field.show field);
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
      Format.printf "Going to create pointer of type: %a\n" StructDef.pp struct_def;
    Format.printf "  root: %a\n" SL.Term.pp_with_sort ctx.root;
    Format.printf "  params: %a\n" SL.Term.pp_with_sort (List.hd values);
    let pto = SL.mk_pto_struct ctx.root struct_def values in
    let es =
      HT.fold
        (fun n sort acc -> (SL.Variable.mk (Format.asprintf "V%d" n) sort) :: acc)
        !existentials []
    in
    SL.mk_exists es @@ SL.mk_star (pto :: ctx.rest)

end

open Result


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
    else HT.find !existentials n
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
  | "eq" ->
    let vars = List.map (parse_var ctx) vars in
    if check_types "eq" vars
    then Result.add_atom ctx @@ SL.mk_eq vars
    else ctx
  | "neq" ->
    let vars = List.map (parse_var ctx) vars in
    if check_types "neq" vars
    then Result.add_atom ctx @@ SL.mk_distinct vars
    else ctx
  | field when PopperEncoding.is_declared_field ctx.input field ->
    let field = PopperEncoding.decode_field field in
    let target = List.nth vars 1 in
    Logger.debug "Adding target %s for %s\n" target field;
    Result.add_field_value ctx field target
  | field when PopperEncoding.is_declared_field_nil ctx.input field ->
    let field = PopperEncoding.decode_field field in
    Logger.debug "Adding target nil for %s\n" field;
    Result.add_field_value ctx field "nil"

  | pred when String.starts_with pred ~prefix:"pred" || ParserContext.is_declared_pred ctx.input pred ->
    (*let params = BatList.take (List.length vars - 1) vars in*)
    Result.add_atom ctx @@ SL.mk_predicate name @@ List.map (parse_var ctx) vars
  | atom ->
    Logger.warning "Ignoring %s during conversion\n" atom;
    ctx

let parse_body ctx params body =
  Format.printf "Parsing body: %s\n" body;
  let weight s =
    if PopperEncoding.is_declared_field ctx.input s
       || PopperEncoding.is_declared_field_nil ctx.input s
    then 0
    else 1
  in
  let aux_cmp s1 s2 = Int.compare (weight s1) (weight s2) in
  BatString.split_on_string ~by:")," body
  |> List.map (fun s -> if not @@ String.ends_with ~suffix:")" s then s ^ ")" else s)
  |> List.sort aux_cmp
  |> List.fold_left parse_atom ctx
  |> Result.gen_body

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

let prefix_re = Str.regexp "^[ \t]*[0-9]+\\(\\.[0-9]+\\)?s[ \t]+"

(** Some versions of Popper prints time marks at the start of lines
    with solution. This functions removes them. *)
let strip_debug_prefix = Str.global_replace prefix_re ""

let solution_re = Str.regexp "\\*+ SOLUTION \\*+[\n]+\\([^*]+\\)[\n]*\\*+"

let convert input ctx popper_out =
  Format.printf "%s" popper_out;
  if BatString.exists popper_out "NO SOLUTION" then None
  else
    let _ = HT.clear !existentials in
    let popper_out = strip_debug_prefix popper_out in
    let _ = Str.search_forward solution_re popper_out 0 in
    let solution = Str.matched_group 1 popper_out in
    let lines = String.split_on_char '\n' solution in
    let precision, recall = parse_stats @@ List.hd lines in
    let res = Result.init input ctx in
    let predicates = parse_predicates res @@ List.tl lines in
    Some SynthesisResult.{precision; recall; concrete_vars = ctx.vars; predicates}
