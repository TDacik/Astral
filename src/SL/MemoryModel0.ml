module Field = struct

  module ID = Identifier.Make ()

  type t = Identifier.t * Sort.t [@@deriving compare, equal]

  let show (id, _) = Identifier.show id

  let get_sort = snd

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare

    module Sort = Sort
    let get_sort = get_sort
  end

  include Datatype.Printable(Self)
  include Datatype.Comparable(Self)
  include Datatype.Collections(Self)
  include SortedBuilder.Make(Self)

  let mk name sort = (ID.mk name, sort)

  let get_name (id, _) = ID.show id

  let is_pointer (_, sort) = Sort.is_loc sort

  let next = mk "next" Sort.loc_ls

  let show_namespace (id, _) = "field_" ^ Identifier.show id

  let smt2_decl (id, sort) =
    Format.asprintf "%s %s"
      (Identifier.show id)
      (Sort.smt2_name sort)

end
