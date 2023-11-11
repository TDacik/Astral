(* Parser context.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module M := Stdlib.Map.Make(String)

type t = {
  (* Declarations *)
  sorts : Sort.Set.t;   (* Uninterpreted sorts *)
  vars : Sort.t M.t;    (* Declared variables *)
  heap_sort : Sort.t;   (* Declared sort of the heap *)

  (* Attributes *)
  expected_status : [ `Sat | `Unsat | `Unknown of string ];
  attributes : String.t M.t;

  (* Options *)
  produce_models : bool;

  (* Assertions *)
  assertions : SSL.t list;
}

val empty : t

val declare_var : t -> string -> Sort.t -> t

val declare_sort : t -> Sort.t -> t

val declare_heap_sort : t -> Sort.t -> t

val type_of_var : t -> string -> Sort.t

val set_expected_status : t -> string -> t

val set_attribute : t -> string -> string -> t

val set_produce_models : t -> bool -> t


val add_assertion : t -> SSL.t -> t

val add_vars : t -> SSL.Variable.t list -> t

val get_vars : t -> SSL.Variable.t list

val get_sl_vars : t -> SSL.Variable.t list

val get_phi : t -> SSL.t

val show : t -> string
