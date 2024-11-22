type loc = Dolmen_std.Loc.t

exception NotSupported of loc * string

exception SortError of loc * string * string * string (* Kind, actual, expected *)

exception NotDeclared of loc * string * string (* Kind, name *)

exception Redeclared of loc * string * string (* Kind, name *)

(* Remove old: *)

exception ParserError of string

exception SortRedefined of Sort.t

exception SortNotDeclared of string

exception VariableRedefined of string

exception StructNotDeclared of string

exception ConstructorNotDeclared of string

exception VariableNotDeclared of string

exception VariableTypeError of SL.Variable.t * Sort.t * Sort.t

(** This type does not have name in Dolmen *)
type dolmen_msg :=
  [  `Advanced of string * (Format.formatter -> unit) *
      (Format.formatter -> unit) * (Format.formatter -> unit)
   | `Regular of Format.formatter -> unit
  ]

val show_msg : dolmen_msg -> string

val parser_error : ?loc:loc -> ?hint:string -> string -> _

val lift_cons : ('a -> 'b) -> string -> 'a list -> 'b
(** Lift unary constructor to lists. *)

val lift_cons2 : ('a -> 'a -> 'b) -> string -> 'a list -> 'b
(** Lift binary constructor to lists. *)

val lift_cons3 : ('a -> 'a -> 'a -> 'b) -> string -> 'a list -> 'b
(** Lift ternary contstructor to lists. *)

val lift_cons4 : ('a -> 'a -> 'a -> 'a -> 'b) -> string -> 'a list -> 'b
(** Lift quaternary constructor to lists. *)

val lift_bitvector_list : string -> (int -> BaseLogic.t list -> BaseLogic.t) -> BaseLogic.t list -> BaseLogic.t
