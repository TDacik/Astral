open Prelude

let escape name =
  if String.contains name ' '
  then "|" ^ name ^ "|"
  else name

type t = Identifier.t * Sort.t

module Make () = struct

  module ID = Identifier.Make () 

  type nonrec t = t

  let get_name var = ID.show @@ fst var

  let get_sort = snd

  let has_sort sort var = Sort.equal sort @@ get_sort var

  let equal (name1, sort1) (name2, sort2) = ID.equal name1 name2 && Sort.equal sort1 sort2

  let compare v1 v2 = if equal v1 v2 then 0 else Stdlib.compare v1 v2

  let mk name sort = (ID.mk @@ escape name, sort)

  let mk_fresh name sort = (ID.mk_fresh @@ escape name, sort)

  let show var = get_name var

  let show_with_sort var = Format.asprintf "%s : %s" (show var) (Sort.show @@ get_sort var)

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end
