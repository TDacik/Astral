(* Representation of constant values *)

module U = UnicodeSymbols
module ID = Identifier.Make ()

type t =
  | Const of Identifier.t * Sort.t
  | Bool of Bool.t
  | Int of Int.t
  | Bitvector of Bitvector.t

  | Tupple of String.t Option.t * t List.t
  | Set of t List.t
  | Array of arr
  [@@deriving equal, compare]

and arr =
  | Enum of t Option.t * (t * t) List.t
  | Identity
  [@@deriving equal, compare]

let rec show = function
  | Const (c, _) -> Identifier.show c
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Bitvector b -> Bitvector.show b
  | Set elems ->
    let body = String.concat ", " @@ List.map show elems in
    "{" ^ body ^ "}"
  | Array (Enum (default, elems)) ->
    let show_elem (x, y) = Format.asprintf "%s %s %s" (show x) !U.maps_to (show y) in
    let body = String.concat ", " @@ List.map show_elem elems in
    begin match default with
    | None -> Format.asprintf "{%s}" body
    | Some x when elems = [] -> Format.asprintf "{_ %s %s}" !U.maps_to (show x)
    | Some x  -> Format.asprintf "{%s, _ %s %s}" body !U.maps_to (show x)
    end
  | Array Identity -> "\\x. x"

module Self = struct
  type nonrec t = t
  let show = show
end

include Datatype.Printable(Self)

let get_sort = function
  | Const (_, sort) -> sort
  | Bool _ -> Sort.bool
  | Int _ -> Sort.int
  | Bitvector (_, width) -> Sort.mk_bitvector width

let mk_bool b = Bool b
let tt = mk_bool true
let ff = mk_bool false

let mk_const sort name = Const (ID.mk name, sort)
let mk_int x = Int x
let mk_bitvector bv = Bitvector bv
let mk_bitvector_of_string str = Bitvector (Bitvector.of_string str)
let mk_bitvector_of_int i width = Bitvector (Bitvector.of_int i width)
let mk_array ?default bindings = Array (Enum (default, bindings))

(* TODO: check existence *)
let array_add_binding arr index value = match arr with
  | Array (Enum (default, bindings)) -> Array (Enum (default, (index, value) :: bindings))

let mk_set elems = Set elems

let is_true = equal tt

let get_int = function Int i -> i

let get_elems = function Set xs -> xs

let select arr x = match arr with
  | Array (Identity) -> x
  | Array (Enum (default, bindings)) ->
    try List.assoc x bindings
    with Not_found -> Option.get default

let rec map fn = function
  | Const _ | Bool _ | Int _ | Bitvector _ as c -> fn c
  | Tupple (name, elems) -> Tupple (name, List.map (map fn) elems)
  | Set (elems) -> Set (List.map (map fn) elems)
  | Array _ -> failwith "TODO: map array"
