(* Representation of logic sorts.
 *
 * TODO: could sort implement logic signature?
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

type t =
  | Bool
  | Int
  | Finite of String.t * string list
  | Set of t
  | Sequence of t
  | Array of t * t
  | Bitvector of int
  | Loc of string  (* Distinguished uninterpreted sort for SL *)
  | Tupple of t list
  | Sum of t list
  | Uninterpreted of string

(* Functional constructors *)
let mk_bool = Bool
let mk_int = Int
let mk_finite name consts = Finite (name, consts)
let mk_set dom = Set dom
let mk_array dom range = Array (dom, range)
let mk_bitvector width = Bitvector width
let mk_loc name = Loc name
let mk_sequence dom = Sequence dom
let mk_uninterpreted name = Uninterpreted name
let mk_sum sorts = Sum sorts

let loc_ls = Loc "LS_t"
let loc_dls = Loc "DLS_t"
let loc_nls = Loc "NLS_t"
let loc_nil = Loc "nil_t"

(** Checks *)
let is_loc = function Loc _ -> true | _ -> false
let is_set = function Set _ -> true | _ -> false
let is_array  = function Array _ -> true | _ -> false

let is_ls = (=) loc_ls
let is_dls = (=) loc_dls
let is_nls = (=) loc_nls
let is_nil = (=) loc_nil

let rec show = function
  | Bool -> "Bool"
  | Int -> "Int"
  | Loc name -> name
  | Finite (name, _) -> name
  | Set (elem_sort) -> Format.asprintf "(Set %s)" (show elem_sort)
  | Sequence (elem_sort) -> Format.asprintf "(Seq %s)" (show elem_sort)
  | Array (dom, range) -> Format.asprintf "(Array %s -> %s)" (show dom) (show range)
  | Bitvector width -> Format.asprintf "(Bitvector %d)" width
  | Tupple sorts -> "(" ^ (String.concat ", " @@ List.map show sorts) ^ ")"
  | Sum sorts -> "(" ^ (String.concat " | " @@ List.map show sorts) ^ ")"
  | Uninterpreted name -> name

let rec show_kind = function
  | Bool -> "bool"
  | Int -> "int"
  | Loc name -> "loc_" ^ name
  | Finite (name, _) -> "finite_" ^ name
  | Array (dom, range) -> "array_" ^ (show_kind dom) ^ "_" ^ (show_kind range)
  | Bitvector width -> "bitvector_" ^ string_of_int width
  | Uninterpreted name -> "uninterpreted_" ^ name

let get_dom_sort = function
  | Set dom_sort -> dom_sort
  | Sequence dom_sort -> dom_sort
  | Array (dom_sort, _) -> dom_sort
  | other -> Utils.internal_error @@ Format.asprintf "Cannot get domain of sort %s" (show other)

let get_range_sort = function Array (_, range_sort) -> range_sort
let get_width = function Bitvector width -> width

let is_atomic = function
  | Bool | Int | Finite _ | Bitvector _ | Loc _ | Uninterpreted _ -> true
  | Set _ | Sequence _ | Array _ | Tupple _ | Sum _ -> false

let rec equal sort1 sort2 = match sort1, sort2 with
  | Bool, Bool -> true
  | Int, Int -> true
  | Finite (name1, _), Finite (name2, _) -> String.equal name1 name2 (* TODO *)
  | Set elem1, Set elem2 -> equal elem1 elem2
  | Sequence dom1, Sequence dom2 -> equal dom1 dom2
  | Array (dom1, range1), Array (dom2, range2) -> equal dom1 dom2 && equal range1 range2
  | Bitvector width1, Bitvector width2 -> Int.equal width1 width2
  | Tupple t1, Tupple t2 -> List.for_all2 equal t1 t2
  | Sum s1, Sum s2 -> List.for_all2 equal s1 s2
  | Loc name1, Loc name2 | Uninterpreted name1, Uninterpreted name2 -> String.equal name1 name2
  | _ -> false

let compare s1 s2 = if equal s1 s2 then 0 else Stdlib.compare s1 s2

let rec substitute input pattern target =
  assert (is_atomic target);
  match pattern, input with
  | (Bool, Bool) | (Int, Int) -> target
  | (Finite (name1, _), Finite (name2, _)) when String.equal name1 name2 -> target
  | (Loc name1, Loc name2) when String.equal name1 name2 -> target
  | (Uninterpreted name1, Uninterpreted name2) when String.equal name1 name2 -> target
  | (Bitvector width1, Bitvector width2) when Int.equal width1 width2 -> target

  | Set (elem_sort), _ -> Set (substitute elem_sort pattern target)
  | Sequence (elem_sort), _ -> Sequence (substitute elem_sort pattern target)
  | Array (dom, range), _ ->
      Array (substitute dom pattern target, substitute range pattern target)
  | Tupple sorts, _ -> Tupple (List.map (fun s -> substitute s pattern target) sorts)
  | Sum sorts, _ -> Sum (List.map (fun s -> substitute s pattern target) sorts)
  | _ -> input

module Self = struct
  type nonrec t = t
  let show = show
  let compare = compare
end

include Datatype.Printable(Self)
include Datatype.Collections(Self)
