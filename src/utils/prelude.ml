(* Use Batteries instead of Stdlib *)

module List = BatList

(* Extended modules *)

module String = struct
  include BatString
  let hash = Hashtbl.hash
end

module Int = struct
  include BatInt
  let hash = Hashtbl.hash
end
