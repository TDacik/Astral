(* Parser for SMT-LIB format
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Dolmen

val parse : Context.t -> ?type_env:TypeEnvironment.t -> string -> Context.t
(** Parse a SMT-LIB body of separation logic formula given as a string. *)

(** {2 Auxilliary functions} *)

val parse_statements : string -> Std.Statement.t list
