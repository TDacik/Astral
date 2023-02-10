(* Parser for (a fragment of) separation logic encoded in SMT-LIB format
 * (see https://sl-comp.github.io/docs/smtlib-sl.pdf).
 *
 * The parser works as follow:
 *  1. splits the input into heap declaration and SMT-LIB compatible rest
 *  2. TODO: parse heap sort from heap declaration
 *  3. parse the rest of the input using SMT-LIB parser
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let split content =
  let heap_decl_re = Str.regexp "\\(.\\|\n\\)*(declare-heap\\(.\\|\n\\)*" in
  let res = Str.string_match heap_decl_re content 0 in
  let start =
    try Str.group_end 1
    with Invalid_argument _ -> failwith "Missing heap declaration"
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
  let re = Str.regexp ("(declare-heap" ^ heap_sort ^ ")") in
  let smt_script = Str.global_replace re "" content in
  (String.trim heap_sort), smt_script

(* TODO *)
let parse_heap_sort content = Sort.mk_array Sort.Loc Sort.Loc

let parse_string str =
  let context = Context.empty in
  let heap, smt_script = split str in
  (* Parse heap sort *)
  let _ = parse_heap_sort heap in
  (* Parse smtlib scripts *)
  SmtlibParser.parse context smt_script

let parse_file path =
  let channel = In_channel.open_text path in
  let content = In_channel.input_all channel in
  In_channel.close channel;
  parse_string content
