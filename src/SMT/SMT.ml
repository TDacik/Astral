let mk_context () = ()

(* The values are explicitly prefixed by SMT_ so they are not confused with Astral's status *)
type status =
  | SMT_Sat
  | SMT_Unsat
  | SMT_Unknown

  (*
module rec Sort = struct

  (* == Enumeration sort == *)

  let mk_enum_sort name constants = Enumeration (name, constants)

  let get_constants = function
    | Enumeration (_, consts) -> List.map (fun c -> Term.Constant c) consts

  (* == Set sort == *)

  let mk_set_sort name elem_sort = Set (name, elem_sort)


end
*)

type sort =
  | Bool
  | Integer
  | Finite of string * term list
  | Set of sort
  | Array of sort * sort

and term =
  | Constant of String.t
  | Variable of String.t * sort

  (* LIA *)
  | Plus of term * term
  | Mult of term * term

  (* Sets *)
  | Membership of term * term
  | Subset of term * term
  | Disjoint of term * term
  | Union of term list * sort (* Is sort necessary *)
  | Inter of term list * sort (* Is sort necessary *)
  | Diff of term * term
  | Compl of term
  | Enumeration of term list * sort

  (* Arrays *)
  | ConstArr of term
  | Select of term * term
  | Store of term * term * term

  (* Boolean *)
  | Equal of term * term
  | Distinct of term list
  | And of term list
  | Or of term list
  | Not of term
  | Implies of term * term
  | Iff of term * term
  | True
  | False

  (* Quantifiers *)
  | Exists of term * term
  | Forall of term * term

(* ==== Syntactic manipulation ==== *)

let rec substitute ?(bounded=[]) phi x term = match phi with
  | Variable (var, sort) ->
      if String.equal (match x with Variable (s, _) -> s) var then term
      else Variable (var, sort)

  (* Quantifiers *)
  | Exists (binder, phi) -> substitute ~bounded:(binder :: bounded) phi x term
  | Forall (binder, phi) -> substitute ~bounded:(binder :: bounded) phi x term

  (* Others --> TODO: *)
  | _ -> phi

module Sort = struct

  type t = sort

  let get_elem_sort = function Set (elem_sort) -> elem_sort
  let get_dom_sort = function Array (dom_sort, _) -> dom_sort
  let get_range_sort = function Array (_, range_sort) -> range_sort

  let rec to_string = function
    | Bool -> "boolean"
    | Integer -> "integer"
    | Finite (name, _) -> "finite " ^ name
    | Set (elem_sort) -> (to_string elem_sort) ^ " set"
    | Array (dom, range) -> Format.asprintf "%s -> %s" (to_string dom) (to_string range)

end

module Term = struct

  type t = term

  let size _ = 0 (* TODO *)

  let rec get_sort = function
    | Variable (_, sort) -> sort

    (* Sets *)
    | Membership _ | Subset _ | Disjoint _ -> Bool
    | Union (_, sort) -> sort
    | Inter (_, sort) -> sort
    | Diff (s1, _) -> get_sort s1
    | Compl s -> get_sort s
    | Enumeration (_, sort) -> sort

    | ConstArr (const) -> failwith "TODO"
    | Select (a, _) -> get_sort a
    | Store (a, _, _) -> get_sort a

    | Equal _ -> Bool
    | Distinct _ -> Bool
    | And _ -> Bool
    | Or _ -> Bool
    | Not _ -> Bool
    | Implies _ -> Bool
    | Iff _ -> Bool
    | True -> Bool
    | False -> Bool

    (* Quantifiers *)
    | Exists _ | Forall _ -> Bool

  let to_string = function
    | Variable (x, sort) -> Format.asprintf "%s : %s" x (Sort.to_string sort)
    | Constant c -> c

end

module Var = struct

  let index = ref (-1)

  let mk name sort =
    if String.contains name ' '
    then Variable ("|" ^ name ^ "|", sort)
    else Variable (name, sort)

  let mk_fresh name sort =
    index := !index + 1;
    Variable (Format.asprintf "%s!%d" name !index, sort)

end

module Equality = struct

  let mk_eq t1 t2 = Equal (t1, t2)
  let mk_distinct ts = Distinct ts

  let mk_var = Var.mk
  let mk_fresh_var = Var.mk_fresh

  let get_sort = Term.get_sort
end

module Boolean = struct

  include Equality

  let mk_false () = False
  let mk_true () = True
  let mk_and ts = And ts
  let mk_or ts = Or ts
  let mk_not t = Not t
  let mk_implies t1 t2 = Implies (t1, t2)
  let mk_iff t1 t2 = Iff (t1, t2)

end

module Enumeration = struct

  let mk_const name = Constant name

  let mk_sort name constants = Finite (name, constants)
  let get_constants = function Finite (_, consts) -> consts

end

module Arithmetics = struct

  let mk_plus t1 t2 = Plus (t1, t2)
  let mk_mult t1 t2 = Mult (t1, t2)

end

module Array = struct

  include Equality

  let mk_sort dom range = Array (dom, range)

  let mk_const t = ConstArr t
  let mk_select arr x = Select (arr, x)

end

module Set = struct

  include Equality
  let get_elem_sort set = match get_sort set with
    | Set (elem_sort) -> elem_sort

  let mk_sort elem_sort = Set elem_sort

  let mk_mem elem set = Membership (elem, set)
  let mk_subset s1 s2 = Subset (s1, s2)
  let mk_disjoint s1 s2 = Disjoint (s1, s2)

  let mk_union ts sort = Union (ts, sort)
  let mk_inter ts sort = Inter (ts, sort)
  let mk_diff t1 t2 = Diff (t1, t2)
  let mk_compl t = Compl t
  let mk_enumeration sort elements = Enumeration (elements, sort)

  let mk_eq_empty set = Equal (set, Enumeration ([], get_sort set))
  let mk_eq_singleton set x = Equal (set, Enumeration ([x], get_sort set))

end

module Quantifier = struct

  let mk_forall x phi = Forall (x, phi)
  let mk_exists x phi = Exists (x, phi)

end

module Model = struct

  include Map.Make(String)

  (*
  include Map.Make(String)
  let eval model = function
    | Constant c ->
    | Variable (name, sort) -> find name

    (* Sets *)
    | Membership (x, set) ->
        if List.mem ...
    | Subset of term * term
    | Disjoint of term * term
    | Union of term list * sort (* Is sort necessary *)
    | Inter of term list * sort (* Is sort necessary *)
    | Diff of term * term
    | Compl of term
    | Enumeration of term list * sort

  (* Arrays *)
  | ConstArr of term
  | Select of term * term
  | Store of term * term * term

  (* Boolean *)
  | Equal of term * term
  | Distinct of term list
  | And of term list
  | Or of term list
  | Not of term
  | Implies of term * term
  | Iff of term * term
  | True
  | False

  (* Quantifiers *)
  | Exists of term * term
  | Forall of term * term
*)
  let get_const_interp_e _ _ = failwith ""
  let evaluate _ _ _ = failwith ""

end


