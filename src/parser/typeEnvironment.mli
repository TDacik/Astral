(* Simple type checking for SSL parsing
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

exception VariableRedefined of string

exception VariableNotDeclared of string

module M := Stdlib.Map.Make(String)

type t = {
  sorts : Sort.Set.t;   (* Uninterpreted sorts *)
  vars : Sort.t M.t;    (* Declared variables *)

  loc_sort : Sort.t;    (* Sort representing locations *)
  heap_sort : Sort.t;   (* Declared sort of the heap *)
}

val empty : t

val declare_var : string -> Sort.t -> t -> t

val declare_sort : Sort.t -> t -> t

val declare_loc_sort : Sort.t -> t -> t

val declare_heap_sort : Sort.t -> t -> t

val type_of_var : string -> t -> Sort.t

val show : t -> string
