(* Parser for (a fragment of) separation logic encoded in SMT-LIB format
 * (see https://sl-comp.github.io/docs/smtlib-sl.pdf).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

val parse_string : string -> Context.t

val parse_file : string -> Context.t
