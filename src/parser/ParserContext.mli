(* Parser context.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open MemoryModel

module S := Set.Make(String)
module M := Map.Make(String)

type t = ParserContext_type.t

val empty :
  ?sorts:Sort.t M.t ->
  ?struct_defs:StructDef.t M.t ->
  ?heap_sort:HeapSort.t ->
  ?ids:S.t
  -> unit
  -> t

val declare_var : t -> string -> Sort.t -> t

val declare_sort : t -> Sort.t -> t

val declare_heap_sort : t -> (Sort.t * StructDef.t) list -> t

val find_sort : t -> string -> Sort.t

val declare_struct : t -> string -> string -> Field.t list -> t

val is_declared_struct : t -> string -> bool

val declare_pred : t -> string -> t

val is_declared_pred : t -> string -> bool

val find_var : t -> string -> SL.Variable.t

val find_struct_def_by_cons : t -> string -> StructDef.t

val find_struct_def_by_name : t -> string -> StructDef.t

val type_of_var : t -> string -> Sort.t

val set_expected_status : t -> string -> t

val set_attribute : t -> string -> string -> t

val set_produce_models : t -> bool -> t


val add_assertion : t -> SL.t -> t

val add_vars : t -> SL.Variable.t list -> t

val get_vars : t -> SL.Variable.t list

val get_sl_vars : t -> SL.Variable.t list

val get_phi : t -> SL.t

val show : t -> string
