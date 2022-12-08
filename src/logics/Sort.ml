type t =
  | Bool
  | Int
  | Finite of String.t * string list
  | Set of t
  | Sequence of t
  | Array of t * t
  | Bitvector of int
  | Loc (* Uninterpreted sort used for separation logic *)

(* Functional constructors *)
let mk_bool = Bool
let mk_int = Int
let mk_finite name consts = Finite (name, consts)
let mk_set dom = Set dom
let mk_array dom range = Array (dom, range)
let mk_bitvector width = Bitvector width
let mk_sequence dom = Sequence dom

let get_dom_sort = function
  | Set dom_sort -> dom_sort
  | Sequence dom_sort -> dom_sort
  | Array (dom_sort, _) -> dom_sort

let get_range_sort = function Array (_, range_sort) -> range_sort
let get_width = function Bitvector width -> width

let rec equal sort1 sort2 = match sort1, sort2 with
  | Bool, Bool -> true
  | Int, Int -> true
  | Loc, Loc -> true
  | Finite (name1, _), Finite (name2, _) -> String.equal name1 name2 (* TODO *)
  | Set elem1, Set elem2 -> equal elem1 elem2
  | Sequence dom1, Sequence dom2 -> equal dom1 dom2
  | Array (dom1, range1), Array (dom2, range2) -> equal dom1 dom2 && equal range1 range2
  | Bitvector width1, Bitvector width2 -> Int.equal width1 width2
  | _ -> false

let compare s1 s2 = if equal s1 s2 then 0 else Stdlib.compare s1 s2

let rec show = function
  | Bool -> "Bool"
  | Int -> "Int"
  | Loc -> "Loc"
  | Finite (name, _) -> name
  | Set (elem_sort) -> Format.asprintf "(Set %s)" (show elem_sort)
  | Sequence (elem_sort) -> Format.asprintf "(Seq %s)" (show elem_sort)
  | Array (dom, range) -> Format.asprintf "(Array %s -> %s)" (show dom) (show range)
  | Bitvector width -> Format.asprintf "(Bitvector %d)" width

module Self = struct
  type nonrec t = t
  let show = show
end

include Datatype.Printable(Self)
