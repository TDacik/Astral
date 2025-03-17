(* Representation of exceptions that may be raised by parser.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

type actual = string
type expected = string
type what =
  | Builtin of what
  | Variable | Sort | Structure | Constructor

type parser_error =
  | SyntaxError of string
  | NotSupported of string
  | SortError of string * actual * expected
  | NotDeclared of what * string
  | Redefined of what * string

type t = Dolmen_std.Loc.t option * ParserContext_type.t option * parser_error

exception ParserError of t

let _raise loc ctx error = raise @@ ParserError (loc, ctx, error)

let raise_syntax_error loc msg = _raise loc None (SyntaxError msg)

let raise_not_supported loc msg = _raise loc None (NotSupported msg)

let raise_not_declared loc ctx what name = _raise loc (Some ctx) @@ NotDeclared (what, name)

let raise_redefined loc ctx what name = _raise loc (Some ctx) @@ Redefined (what, name)

let raise_sort_error loc ~name ~actual ~expected = _raise loc None @@ SortError (name, actual, expected)
