(* Parser for (a fragment of) separation logic encoded in SMT-LIB format
 * (see https://sl-comp.github.io/docs/smtlib-sl.pdf).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

val pretty_error : ParserException.t -> unit

val parse_file :  string -> ParserContext.t
