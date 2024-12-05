module Field = MemoryModel0.Field

module StructDef = struct

  module ID = Identifier.Make ()

  type t = {
    name : Identifier.t;
    cons : Identifier.t;
    fields : Field.t list;
  }

  let mk name cons fields = {
    name = ID.mk name;
    cons = ID.mk cons;
    fields = fields;
  }

  let compare s1 s2 = Identifier.compare s1.name s2.name
  let equal s1 s2 = Identifier.equal s1.name s2.name

  let ls = mk "LS_t" "c_ls" [Field.mk "next" Sort.loc_ls]

  let signature def = List.map Field.get_sort def.fields

  let mk_tuple n =
    let sort = Sort.mk_loc ("tuple_" ^ string_of_int n) in
    let name = "tuple_" ^ string_of_int n in
    let cons = name ^ "_c" in
    let fields = List.init n (fun i -> Field.mk ("f_" ^ string_of_int i) sort) in
    mk name cons fields

  let lift_sort sort =
    let sort_name = Sort.name sort in
    let name = Format.asprintf "%s_wrapper" sort_name in
    let cons = sort_name ^ "_c" in
    let field = Field.mk ("next") sort in (* TODO: should be qualified? *)
    mk name cons [field]

  let get_name def = Identifier.show def.name

  let get_constructor def = Identifier.show def.cons

  let get_fields def = def.fields

  let field_index def field =
    Option.get @@ List.find_index (Field.equal field) def.fields

  let get_sorts def =
    List.map Field.get_sort def.fields
    |> BatList.unique ~eq:Sort.equal

  let show self =
    Format.asprintf "%s := %s(%s)"
      (Identifier.show self.name)
      (Identifier.show self.cons)
      (Field.show_list self.fields)

  let show_cons self = Identifier.show self.cons

  let smt2_decl self =
    Format.asprintf "(declare-datatype %s ((%s %s)))"
      (Identifier.show self.name)
      (Identifier.show self.cons)
      (String.concat " " @@
       List.map (fun f -> Format.asprintf "(%s)" (Field.smt2_decl f)) self.fields)

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end
