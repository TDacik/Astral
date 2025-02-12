type loc := Dolmen_std.Loc.t

(** This type does not have name in Dolmen *)
type dolmen_msg :=
  [  `Advanced of string * (Format.formatter -> unit) *
      (Format.formatter -> unit) * (Format.formatter -> unit)
   | `Regular of Format.formatter -> unit
  ]

val show_msg : dolmen_msg -> string

val lift_cons : ?loc:loc -> ('a -> 'b) -> string -> 'a list -> 'b
(** Lift unary constructor to lists. *)

val lift_cons2 : ?loc:loc -> ('a -> 'a -> 'b) -> string -> 'a list -> 'b
(** Lift binary constructor to lists. *)

val lift_cons3 : ?loc:loc -> ('a -> 'a -> 'a -> 'b) -> string -> 'a list -> 'b
(** Lift ternary contstructor to lists. *)

val lift_cons4 : ?loc:loc -> ('a -> 'a -> 'a -> 'a -> 'b) -> string -> 'a list -> 'b
(** Lift quaternary constructor to lists. *)

val lift_bitvector_list : ?loc:loc -> string -> (int -> BaseLogic.t list -> BaseLogic.t) -> BaseLogic.t list -> BaseLogic.t
