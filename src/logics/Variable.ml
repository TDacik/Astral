open Prelude

let escape name =
  if String.contains name ' '
  then "|" ^ name ^ "|"
  else name

type t = string * Sort.t

module Make ( ) = struct

  (* ==== Index managment ==== *)

  module Indexes = struct

    module HT = Hashtbl.Make(String)

    let hash_table = ref (HT.create 97)

    let mk_fresh_name orig =
      let index =
        try HT.find !hash_table orig
        with Not_found -> 0
      in
      HT.replace !hash_table orig (index + 1);
      Format.asprintf "%s!%d" orig index

  end

  type nonrec t = t

  let get_name = fst

  let get_sort = snd

  let has_sort sort var = Sort.equal sort @@ get_sort var

  let equal (name1, sort1) (name2, sort2) = String.equal name1 name2 && Sort.equal sort1 sort2

  let compare v1 v2 = if equal v1 v2 then 0 else Stdlib.compare v1 v2

  let mk name sort = (escape name, sort)

  let mk_fresh name sort = (Indexes.mk_fresh_name (escape name), sort)

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
