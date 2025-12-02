(* Use Batteries instead of Stdlib *)

module List = BatList

(* Extended modules *)

module String = struct
  include BatString
  let hash = Hashtbl.hash
  let show = Fun.id
end

module Int = struct
  include BatInt
  let hash = Hashtbl.hash
  let show = string_of_int
end
