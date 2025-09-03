(* Parser context.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open MemoryModel

module S := Set.Make(String)
module M := Map.Make(String)

type loc := Dolmen_std.Loc.t

type t = ParserContext_type.t

val empty :
  ?sorts:Sort.t M.t ->
  ?struct_defs:StructDef.t M.t ->
  ?heap_sort:HeapSort.t ->
  ?ids:S.t
  -> unit
  -> t

val add_defs : t -> t -> t
(** Combine two compatible contexts *)

val declare_var : ?loc:loc -> t -> string -> Sort.t -> t

val declare_sort : ?loc:loc -> t -> Sort.t -> t

val declare_heap_sort : t -> (Sort.t * StructDef.t) list -> t

val find_sort : ?loc:loc -> t -> string -> Sort.t

val declare_struct : ?loc:loc -> t -> string -> string -> Field.t list -> t

val is_declared_struct : t -> string -> bool

val declare_pred : t -> string -> t

val is_declared_pred : t -> string -> bool

val find_var : ?loc:loc -> t -> string -> SL.Variable.t

val find_struct_def_by_cons : ?loc:loc -> t -> string -> StructDef.t

val find_struct_def_by_name : ?loc:loc -> t -> string -> StructDef.t

val type_of_var : ?loc:loc -> t -> string -> Sort.t

val set_expected_status : t -> string -> t

val set_attribute : t -> string -> string -> t

val set_produce_models : t -> bool -> t


val add_assertion : t -> SL.t -> t

val add_vars : t -> SL.Variable.t list -> t

(** {2 Accessors} *)

val get_vars : t -> SL.Variable.t list

val get_sl_vars : t -> SL.Variable.t list

val get_phi : t -> SL.t

val get_sorts : t -> Sort.t list

val get_struct_defs : t -> StructDef.t list

val get_heap_sort : t -> HeapSort.t

val get_predicates : t -> String.t list


val show : t -> string
