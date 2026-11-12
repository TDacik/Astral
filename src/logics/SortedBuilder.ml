open Logic_sig

module Make (Input : SORTED_BASE) = struct

  include Input

  let has_sort sort term = Input.Sort.equal sort @@ get_sort term

  let show_with_sort term = Format.asprintf "%s : %s" (show term) (Input.Sort.show @@ get_sort term)

  let pp_with_sort fmt term = Format.fprintf fmt "%s" (show_with_sort term)

end
