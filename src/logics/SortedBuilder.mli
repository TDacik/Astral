open Logic_sig

module Make (In : SORTED_BASE) : SORTED
  with type t := In.t
   and type sort := In.Sort.t
