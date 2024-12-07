(* Parser.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Astral
open ParserUtils

let parse path =
  Profiler.add "Parsing";
  try
    let input = Parser.parse_file path in
    Debug.input input;
    input
  with
  | Dolmen_std.Loc.Syntax_error (position, msg) ->
    let file = Dolmen_std.Loc.mk_file path in
    let loc = Dolmen_std.Loc.loc file position in
    let error_msg =
      Format.asprintf "Syntax error at %a\n%s" Dolmen_std.Loc.fmt loc
        (ParserUtils.show_msg msg)
    in
    Utils.error error_msg
  | VariableRedefined x -> parser_error ("Variable " ^ x ^ " redefined")
  | VariableNotDeclared x -> parser_error ("Variable " ^ x ^ " is not declared")
  | ConstructorNotDeclared x -> parser_error ("Constructor " ^ x ^ " is not declared")
  | StructNotDeclared x -> parser_error ("Structure " ^ x ^ " is not declared")
  | SortNotDeclared x -> parser_error ("Sort " ^ x ^ " is not declared")

  (* New *)
  | SortError (loc, name, expected, actual) ->
    parser_error ~loc
      (Format.asprintf "Application '%s' expects %s, but got:\n  %s" name expected actual)
  | SyntaxError msg ->
    parser_error msg
