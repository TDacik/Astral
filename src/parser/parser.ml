(* Parser for (a fragment of) separation logic encoded in SMT-LIB format
 * (see https://sl-comp.github.io/docs/smtlib-sl.pdf).
 *
 * The parser works as follow:
 *  1. splits the input into heap declaration and SMT-LIB compatible rest
 *  2. TODO: parse heap sort from heap declaration
 *  3. parse the rest of the input using SMT-LIB parser
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open ParserUtils

let split content =
  let heap_decl_re = Str.regexp "\\(.\\|\n\\)*(declare-heap\\(.\\|\n\\)*" in
  let res = Str.string_match heap_decl_re content 0 in
  let start =
    try Str.group_end 1
    with Invalid_argument _ -> raise @@ ParserError "Missing heap declaration"
  in
  let worklist = BatString.lchop ~n:(start+13) content in
  let _, heap_sort = String.fold_left (fun (depth, acc) c ->
    let depth' = match c with
      | '(' -> depth + 1
      | ')' -> depth - 1
      | _ -> depth
    in
    if depth' <= 0 then (-1, acc)
    else (depth', acc ^ BatString.of_char c)
  ) (1, "") worklist
  in
  let before = BatString.left content start in
  let after = BatString.lchop ~n:(start + 13 + 1 + BatString.length heap_sort) content in
  (before, heap_sort, after)

let parse_string str =
  let decls, heap_decl, rest = split str in
  let ctx = SmtlibParser.parse decls in
  let ctx = HeapParser.parse ctx heap_decl in
  SmtlibParser.parse ~ctx rest

let parse_file path =
  let channel = In_channel.open_text path in
  let content = In_channel.input_all channel in
  In_channel.close channel;
  parse_string content
