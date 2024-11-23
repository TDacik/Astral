module Field = struct

  module ID = Identifier.Make ()

  type t = Identifier.t * Sort.t

  (* TODO: compare also sorts? *)
  let compare (id1, sort1) (id2, sort2) = Identifier.compare id1 id2

  let mk name sort = (ID.mk name, sort)

  let get_sort = snd

  let show (id, _) = Identifier.show id

  let show_with_sort (id, sort) =
    Format.asprintf "%s : %s"
      (Identifier.show id)
      (Sort.show sort)

  let next = mk "next" Sort.loc_ls

  let smt2_decl (id, sort) =
    Format.asprintf "%s %s"
      (Identifier.show id)
      (Sort.name sort)

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Comparable(Self)
  include Datatype.Collections(Self)

end
