(* Conversion of stack-heap models into Prolog.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open MemoryModel
open StackHeapModel

open Value
open Location

let cnt : int ref = ref (-1)

let next () = cnt := !cnt + 1

let var_to_string var =
  SL.Variable.show var
  |> BatString.capitalize_ascii
  |> (fun s -> BatString.nreplace ~sub:"!" ~by:"" ~str:s)

let term_to_string term =
  SL.Term.as_var term
  |> var_to_string

let tuple printer = function
  | [x] -> Format.sprintf "%s," (printer x)
  | xs -> String.concat "," @@ List.map printer xs

let id_to_prolog id =
  let open InductiveDefinition in
  let id = InductiveDefinition.map_cases RemoveVariadic.apply id in
  let header = Format.sprintf "%s(%s)" id.name (tuple var_to_string id.header) in
  let cases = InductiveDefinition.cases id in
  let convert_case case =
    let _, atoms = SL.as_quantified_symbolic_heap case in
    let pred_cmp x y =
      if SL.is_predicate x then 1
      else if SL.is_predicate y then -1
      else SL.compare x y
    in
    let atoms = List.sort pred_cmp atoms in
    let convert_atom a = match SL.view a with
      | Eq [x; y] -> Some (Format.sprintf "eq(%s, %s)" (term_to_string x) (term_to_string y))
      | Distinct [x; y] -> Some (Format.sprintf "neq(%s, %s)" (term_to_string x) (term_to_string y))
      | PointsTo (x, def, ys) -> Some (
          let fields = StructDef.get_fields def in
          String.concat ", " @@ List.mapi (fun i f -> Format.sprintf "%s(%s, %s)" (Field.show f) (term_to_string x) (term_to_string @@ List.nth ys i)) fields
        )
      | Predicate (name, xs, 0, []) -> Some (Format.sprintf "%s(%s)" name (tuple term_to_string xs))
      | _ -> failwith @@ SL.show a
    in
    String.concat ", " @@ List.filter_map convert_atom atoms
  in
  String.concat "\n" @@ List.map (fun case ->
    Format.sprintf "%s :- %s." header (convert_case case)
  ) cases


module Result = struct

  type atom = string * string list

  type t = {
    knowledge : string list;
    examples: string list;
  }

  let empty = {knowledge = []; examples= []}

  let add_knowledges res atom = {res with knowledge = atom @ res.knowledge}

  let add_example res atom = {res with examples = atom :: res.examples}

  let replace ~sub ~by str = snd @@ BatString.replace ~sub ~by ~str

  let gen_fields fields =
    String.concat "\n" @@ List.map (fun f ->
      let disc = Format.sprintf ":- discontiguous %s/2." (MemoryModel.Field.show f) in
      Format.sprintf "%s\n%s_is_nil(X) :- %s(X, Y), is_null(Y)."
        disc
        (MemoryModel.Field.show f)
        (MemoryModel.Field.show f)
    ) fields

  let gen_preds preds =
    String.concat "\n\n" @@ List.map id_to_prolog preds

  let generate_knowledge preds self fields =
    let heap = String.concat "\n" self.knowledge in
    let fields = gen_fields fields in
    let template = [%blob "bk_template.pl"] in
    template
    |> replace ~sub:"__HEAP_DEFS__" ~by:heap
    |> replace ~sub:"__PREDICATE_DEFS__" ~by:(gen_preds preds)
    |> replace ~sub:"__FIELD_DEFS__" ~by:fields

  let generate_examples self =
    String.concat "\n" self.examples

end

(** Conversions *)

let rename str = Format.asprintf "loc%s_%d" str !cnt

let convert_loc loc = match fst loc with
  | Implicit n -> Format.asprintf "loc%d_%d" n !cnt
  | SMT const -> failwith "Not implemented: smt locations"

let convert_value = function
  | Struct (def, locs) -> List.combine def.fields locs
  | Data _ -> failwith "Not implemented: data values"

(** Prolog generation *)

let generate_one source =
  List.map (fun (field, target) ->
    Format.asprintf "%s(%s, %s)."
      (Field.show field)
      (convert_loc source)
      (convert_loc target)
  )

let generate_ptr sh loc value res =
  let v = convert_value value in
  Result.add_knowledges res @@ generate_one loc v

let generate_descr name res sh phi vars =
  let locs =
    List.map (StackHeapModel.eval_var sh) vars
    |> List.map Location.show
    |> List.map rename
    |> String.concat ", "
  in
  Result.add_example res @@ Format.asprintf "pos(%s(%s))." name locs

let convert name vars res sh phi =
  next ();
  let nil = StackHeapModel.get_nil sh in
  let res = Result.add_knowledges res [Format.sprintf "is_null(%s)." (convert_loc nil)] in
  let res = Heap.fold (generate_ptr sh) sh.heap res in
  let res = generate_descr name res sh phi vars in
  res
