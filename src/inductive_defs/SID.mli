(* Operations over system of inductive definitions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open ID_sig

module ID : sig

  type t =
    | Builtin of (module BUILTIN)
    | UserDefined of InductiveDefinition.t

  val name : t -> string

  val show : t -> string

  val to_def : t -> InductiveDefinition.t

end

type t

include Datatype_sig.PRINTABLE with type t := t

val empty : t

val of_list : InductiveDefinition.t list -> t

val register_builtin : t -> (module BUILTIN) -> t

val register_user_defined : t -> InductiveDefinition.t -> t

val update_user_defined : t -> InductiveDefinition.t -> t

val compute_graph : t -> t

val is_builtin : t -> string -> bool

val is_user_defined : t -> string -> bool

val find : t -> string -> ID.t

val find_builtin : t -> string -> (module BUILTIN)

val find_user_defined : t -> string -> InductiveDefinition.t

val find_first_user_defined : t -> (string -> InductiveDefinition.t -> bool) -> InductiveDefinition.t

val get_user_defined : t -> InductiveDefinition.t list

val get_builtin : t -> (module BUILTIN) list

val filter_map : (ID.t -> ID.t option) -> t -> t

val fold : (ID.t -> 'a -> 'a) -> t -> 'a -> 'a

val fold_builtin : ((module BUILTIN) -> 'a -> 'a) -> t -> 'a -> 'a

val fold_user_defined : (InductiveDefinition.t -> 'a -> 'a) -> t -> 'a -> 'a

(** {2 Unfolding} *)

val unfold : t -> string -> SL.Term.t list -> UnfoldingBound.t -> SL.t

(** {2 Predicate dependencies} *)

val dependency_graph : t -> DependencyGraph.t

val is_self_recursive : t -> string -> bool
(** True if inductive definition contains a recursive call of itself. *)

val dependencies : t -> string -> InductiveDefinition.t list
(** For an user-defined predicate, return it and all the predicates it can directly and
    indirectly call. *)
