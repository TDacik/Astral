(* Representation of logic sorts.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

module Aliases = Identifier.MonoMap(Identifier)

let aliases = ref Aliases.empty

module ID = Identifier.Make()

type t =
  | Bool
  | Int
  | Finite of Identifier.t * string list
  | Set of t
  | Sequence of t
  | Array of t * t
  | Bitvector of int
  | Tupple of t list
  | Sum of t list
  | Uninterpreted of Identifier.t

  (** Distinguished location sort, each can have multiple aliases. *)
  | Loc of Identifier.t * Identifier.t list

  (** Name, constructor, fields
  | Struct of Identifier.t * Constructor.t * (Field.t * t) list *)

(* Functional constructors *)
let bool = Bool
let int = Int
let mk_finite name consts = Finite (ID.mk name, consts)
let mk_set dom = Set dom
let mk_array dom range = Array (dom, range)
let mk_bitvector width = Bitvector width
let mk_sequence dom = Sequence dom
let mk_uninterpreted name = Uninterpreted (ID.mk name)
let mk_sum sorts = Sum sorts

let mk_loc ?(aliases=[]) name =
  Loc (ID.mk name, List.map ID.mk aliases)

let loc_ls  = mk_loc "Loc" ~aliases:["LS_t"; "RefSll_t"]
let loc_nil = mk_loc "nil_t" ~aliases:[]

let rec equal sort1 sort2 = match sort1, sort2 with
  | Bool, Bool -> true
  | Int, Int -> true
  | Finite (name1, _), Finite (name2, _) -> Identifier.equal name1 name2 (* TODO *)
  | Set elem1, Set elem2 -> equal elem1 elem2
  | Sequence dom1, Sequence dom2 -> equal dom1 dom2
  | Array (dom1, range1), Array (dom2, range2) -> equal dom1 dom2 && equal range1 range2
  | Bitvector width1, Bitvector width2 -> Int.equal width1 width2
  | Tupple t1, Tupple t2 -> List.for_all2 equal t1 t2
  | Sum s1, Sum s2 -> List.for_all2 equal s1 s2
  | Uninterpreted name1, Uninterpreted name2 -> Identifier.equal name1 name2
  | Loc (name1, aliases1), Loc (name2, aliases2) ->
    Identifier.equal name1 name2
    || BatList.mem_cmp Identifier.compare name1 aliases2
    || BatList.mem_cmp Identifier.compare name2 aliases1
  | _ -> false

let compare s1 s2 = if equal s1 s2 then 0 else Stdlib.compare s1 s2

(** Checks *)
let is_bool = function Bool -> true | _ -> false
let is_loc = function Loc _ -> true | _ -> false
let is_set = function Set _ -> true | _ -> false
let is_array  = function Array _ -> true | _ -> false
let is_bitvector = function Bitvector _ -> true | _ -> false

let is_nil = (=) loc_nil

let rec show = function
  | Bool -> "Bool"
  | Int -> "Int"
  | Loc (name, _) -> Format.asprintf "(Loc %s)" (Identifier.show name)
  | Finite (name, _) -> Identifier.show name
  | Set (elem_sort) -> Format.asprintf "(Set %s)" (show elem_sort)
  | Sequence (elem_sort) -> Format.asprintf "(Seq %s)" (show elem_sort)
  | Array (dom, range) -> Format.asprintf "(Array %s -> %s)" (show dom) (show range)
  | Bitvector width -> Format.asprintf "(Bitvector %d)" width
  | Tupple sorts -> "(" ^ (String.concat ", " @@ List.map show sorts) ^ ")"
  | Sum sorts -> "(" ^ (String.concat " | " @@ List.map show sorts) ^ ")"
  | Uninterpreted name -> Identifier.show name

let name = function
  | Loc (name, _) -> Identifier.show name
  | Bitvector width -> Format.asprintf "Bitvector%d" width
  | other -> show other

let all_names = function
  | Loc (name, aliases) -> List.map Identifier.show (name :: aliases)
  | other -> [name other]

let to_smt2_decl = function
  | sort -> Format.asprintf "(declare-sort %s 0)" (name sort)

let cardinality = function
  | Bool -> Some 2
  | Finite (_, cs) -> Some (List.length cs)
  | Bitvector width -> Some (Int.of_float @@ 2. ** (Int.to_float width))
  | Int -> None
  | Loc _ -> None

let get_dom_sort = function
  | Set dom_sort -> dom_sort
  | Sequence dom_sort -> dom_sort
  | Array (dom_sort, _) -> dom_sort
  | other -> raise @@ Invalid_argument (Format.asprintf "Sort.get_dom_sort : " ^ show other)

let get_range_sort = function
  | Array (_, range_sort) -> range_sort
  | other -> raise @@ Invalid_argument (Format.asprintf "Sort.get_range_sort : " ^ show other)

let get_width = function
  | Bitvector width -> width
  | other -> raise @@ Invalid_argument (Format.asprintf "Sort.get_width : " ^ show other)

let get_constant_names = function Finite (_, names) -> names

let is_atomic = function
  | Bool | Int | Finite _ | Bitvector _ | Loc _ | Uninterpreted _ -> true
  | Set _ | Sequence _ | Array _ | Tupple _ | Sum _ -> false

let rec substitute input pattern target =
  assert (is_atomic target);
  match pattern, input with
  | (Bool, Bool) | (Int, Int) -> target
  | (Finite (name1, _), Finite (name2, _)) when Identifier.equal name1 name2 -> target
  | (Loc (name1, _), Loc (name2, _)) when Identifier.equal name1 name2 -> target
  | (Uninterpreted name1, Uninterpreted name2) when Identifier.equal name1 name2 -> target
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
