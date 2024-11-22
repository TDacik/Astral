(* Parser for (a fragment of) separation logic encoded in SMT-LIB format
 * (see https://sl-comp.github.io/docs/smtlib-sl.pdf).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

val parse_string : ?filename:string -> string -> ParserContext.t

val parse_file :  string -> ParserContext.t
