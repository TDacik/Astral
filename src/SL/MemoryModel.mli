
open Logic_sig
open Datatype_sig

module Field : sig
  type t = MemoryModel0.Field.t

  val mk : string -> Sort.t -> t

  val next : t
  (** Built-in next field of sort Loc. *)

  val smt2_decl : t -> string

  include SORTED with type t := t and module Sort := Sort

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t
  include COLLECTIONS with type t := t

end

module StructDef : sig

  type t = private {
    name : Identifier.t;
    cons : Identifier.t;
    fields : Field.t list;
  }

  val mk : string -> ?cons:string -> Field.t list -> t
  (** [mk struct_name constructor_name fields] creates a structure consisting
      of given fields. Constructor name is used only for pretty-printing of
      pointer assertions and, by default, is the same as structure name. *)

  val mk_tuple : int -> t
  (** [mk_tuple n] creates a structure consisting of fields f_1, f_2, ..., f_n;
      each of them of sort "tuple_n".

      For internal usage only. *)

  val lift_sort : Sort.t -> t
  (** Creates a structure consisting of single "next" field of provided sort.

      For internal usage only. *)

  val ls : t

  val get_name : t -> string
  val get_constructor : t -> string
  val get_fields : t -> Field.t list

  val signature : t -> Sort.t list
  (** Get list of sorts of all fields in the structure. *)

  val get_sorts : t -> Sort.t list
  (** Same as signature, but removes duplicates. *)

  val field_index : t -> Field.t -> int
  (** Return index of the field in the structure. *)



  val show_cons : t -> string

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t
  include COLLECTIONS with type t := t

end
