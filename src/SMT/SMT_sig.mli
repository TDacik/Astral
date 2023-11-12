open Logic_sig
open Datatype_sig

module type SMT_TERM = sig

  type t

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t

  include LOGIC with type t := t
  (** Basic functions over logical formulae *)

  val mk_eq : t -> t -> t

  val mk_eq_list : t list -> t

  val mk_distinct : t -> t -> t

  val mk_distinct_list : t list -> t

  (* TODO: the following functions can be probably moved to LOGIC *)

  val map : (t -> t) -> t -> t

  val map_vars : (string -> Sort.t -> t) -> t -> t

end
