(* Parser for SMT-LIB format
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Dolmen

val parse : ?ctx:ParserContext.t -> string -> ParserContext.t
(** Parse a SMT-LIB body of separation logic formula given as a string. *)

(** {2 Auxiliary functions} *)

val parse_sort : ParserContext.t -> Std.Term.t -> Sort.t

val parse_statements : string -> Std.Statement.t list
