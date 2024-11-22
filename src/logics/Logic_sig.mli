(* Logic signatures.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Datatype_sig

module type SORT = sig

  type t

  val show : t -> string

  val compare : t -> t -> int

  val equal : t -> t -> bool

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t
  include COLLECTIONS with type t := t

end

module type SORTED = sig

  module Sort : SORT

  type t

  val get_sort : t -> Sort.t

  val has_sort : Sort.t -> t -> bool

  val show_with_sort : t -> string

end



module type VARIABLE = sig

  type t

  module Sort : SORT

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t
  include COLLECTIONS with type t := t

  include SORTED with type t := t and module Sort := Sort

  val mk : string -> Sort.t -> t

  val mk_fresh : string -> Sort.t -> t

  val refresh : t -> t

  val get_name : t -> string

  val describe : t -> string * Sort.t

  val show : t -> string

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : t -> int

end

module type LOGIC = sig

  module Sort : SORT
  module Variable : VARIABLE with module Sort = Sort

  type t

  type term

  (*type view

  val view : t -> view
  *)

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t
  include COLLECTIONS with type t := t

  include SORTED with type t := t and module Sort := Sort

  val hash : t -> int

  val mk_var : string -> Sort.t -> t

  val mk_fresh_var : string -> Sort.t -> t

  val get_all_sorts : t -> Sort.t list

  val of_var : Variable.t -> t

  val of_const : Constant.t -> t

  val get_sort_in_term : string -> t -> Sort.t

  val get_operands : t -> t list

  val is_quantifier_free : t -> bool

  val show : t -> string

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val map : (t -> t) -> t -> t

  (*val map_view : (view -> t) -> t -> t*)

  val get_vars : t -> Variable.t list
  (** Return all variables, without duplicates. *)

  val free_vars : t -> Variable.t list

  val rename_var : string -> string -> t -> t

  val select_subformulae : (t -> bool) -> t -> t list

  val substitute : t -> var:Variable.t -> by:term -> t

  val substitute_list : t -> vars:Variable.t list -> by:term list -> t

  val replace_subformula : t -> subformula:t -> by:t -> t

  val size : t -> int

  type ast

  val to_ast : ?dagify:bool -> t -> ast

  val output_ast : string -> ast -> unit

  val output_benchmark : string -> t -> [`Sat | `Unsat | `Unknown] -> unit

  val (===) : t -> t -> bool
end

module type WITH_VIEW = sig

  type t

  type view

  val view : t -> view

  val map_view : (view -> t) -> t -> t

end
