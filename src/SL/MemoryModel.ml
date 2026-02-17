module Field = MemoryModel0.Field

module StructDef = struct

  module ID = Identifier.Make ()

  type t = {
    name : Identifier.t;
    cons : Identifier.t;
    fields : Field.t list;
  }

  let mk name ?cons fields = {
    name = ID.mk name;
    cons = ID.mk (Option.value cons ~default:name);
    fields = fields;
  }

  let compare s1 s2 = Identifier.compare s1.name s2.name
  let equal s1 s2 = Identifier.equal s1.name s2.name

  let ls = mk "LS" ~cons:"c_ls" [Field.mk "field_next" Sort.loc_ls]

  let signature def = List.map Field.get_sort def.fields

  let mk_tuple n =
    let sort = Sort.mk_loc ("tuple_" ^ string_of_int n) in
    let name = "tuple_" ^ string_of_int n in
    let cons = name ^ "_c" in
    let fields = List.init n (fun i -> Field.mk ("f_" ^ string_of_int i) sort) in
    mk name ~cons fields

  let lift_sort sort =
    let sort_name = Sort.name sort in
    let name = Format.asprintf "%s_wrapper" sort_name in
    let cons = sort_name ^ "_c" in
    let field = Field.mk ("field_next") sort in (* TODO: should be qualified? *)
    mk name ~cons [field]

  let get_name def = Identifier.show def.name

  let get_constructor def = Identifier.show def.cons

  let get_fields def = def.fields

  let field_index def field =
    Option.get @@ List.find_index (Field.equal field) def.fields

  let field_value def field ys =
    let index = field_index def field in
    List.nth ys index

  let find_field fn def = List.find fn def.fields

  let get_sorts def =
    List.map Field.get_sort def.fields
    |> Sort.MonoList.unique

  let show self =
    Format.asprintf "%s := %s(%s)"
      (Identifier.show self.name)
      (Identifier.show self.cons)
      (Field.show_list self.fields)

  let show_cons self = Identifier.show self.cons

  let decl_aux self =
    Format.asprintf "((%s %s))"
      (Identifier.show self.cons)
      (String.concat " " @@
       List.map (fun f -> Format.asprintf "(%s)" (Field.smt2_decl f)) self.fields)

  let smt2_decl self =
    Format.asprintf "(declare-datatype %s %s)"
      (Identifier.show self.name)
      (decl_aux self)

  let smt2_decl_group = function
    | [] -> ""
    | [x] -> smt2_decl x
    | group ->
      let decl_header def = Format.asprintf "(%s 0)" (Identifier.show def.name) in
      let group_header = String.concat "\n    " @@ List.map decl_header group in
      let group_decls = String.concat "\n    " @@ List.map decl_aux group in
      Format.asprintf "(declare-datatypes\n  (\n    %s\n  )\n  (\n    %s\n  )\n)"
        group_header
        group_decls

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end
