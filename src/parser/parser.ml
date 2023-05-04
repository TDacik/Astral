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

let parse_string str =
  let context = Context.empty in
  let heap, smt_script = split str in
  (* Parse smtlib scripts *)
  let context = SmtlibParser.parse context smt_script in
  (* Parse heap sort *)
  let type_env = HeapParser.parse context.type_env heap in
  let context = SmtlibParser.parse Context.empty ~type_env smt_script in
  let context = {context with type_env = type_env} in (* TODO: checl *)
  let context = HeapParser.preprocess_loc_vars context in
  (*Format.printf "Location variables:\n";
  List.iter (fun v -> Format.printf "  %a\n" SSL.Variable.pp v) context.vars_orig;
  Format.printf "SMT variables:\n";
  List.iter (fun v -> Format.printf "  %a\n" SSL.Variable.pp v) context.smt_vars;
  *)
  {context with type_env = type_env}

let parse_file path =
  let channel = In_channel.open_text path in
  let content = In_channel.input_all channel in
  In_channel.close channel;
  parse_string content
