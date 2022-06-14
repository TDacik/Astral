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
  | Integer of string
  | Finite of string * term list
  | Set of string * sort
  | Array of string * sort * sort
  | Dummy (* TODO: remove *)

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
  | Union of term list
  | Inter of term list
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
      if String.equal x var then term
      else Variable (var, sort)

  (* Quantifiers *)
  | Exists (binder, phi) -> substitute ~bounded:(binder :: bounded) phi x term
  | Forall (binder, phi) -> substitute ~bounded:(binder :: bounded) phi x term

  (* Others --> TODO: *)
  | _ -> phi

module Sort = struct

  type t = sort

  let to_string t = "(TODO)"

end

module Term = struct

  type t = term

  let size _ = 0 (* TODO *)


  let to_string t = "(TODO)"

end

module Var = struct

  let index = ref (-1)

  let mk name sort = Variable (name, sort)

  let mk_fresh name sort =
    index := !index + 1;
    Variable (Format.asprintf "%s#%d" name !index, sort)

end

module Equality = struct

  let mk_eq t1 t2 = Equal (t1, t2)
  let mk_distinct ts = Distinct ts

  let mk_var = Var.mk
  let mk_fresh_var = Var.mk_fresh

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

  let mk_sort name dom range = Array (name, dom, range)

  let mk_const t = ConstArr t
  let mk_select arr x = Select (arr, x)

end

module Set = struct

  include Equality

  let mk_sort name elem_sort = Set (name, elem_sort)

  let mk_mem elem set = Membership (elem, set)
  let mk_subset s1 s2 = Subset (s1, s2)
  let mk_disjoint s1 s2 = Disjoint (s1, s2)

  let mk_union ts = Union ts
  let mk_inter ts = Inter ts
  let mk_diff t1 t2 = Diff (t1, t2)
  let mk_compl t = Compl t
  let mk_enumeration sort elements = Enumeration (elements, sort)

  let mk_eq_empty set = Equal (set, Enumeration ([], Dummy))
  let mk_eq_singleton set x = Equal (set, Enumeration ([x], Dummy))

end

module Quantifier = struct

  let mk_forall x phi = Forall (x, phi)
  let mk_exists x phi = Exists (x, phi)

end

module Model = struct
  type t
  let get_const_interp_e _ _ = None
  let evaluate _ _ _ = None
end


