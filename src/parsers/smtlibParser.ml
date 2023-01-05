(* Parser for smt2lib format
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Dolmen
open Dolmen_smtlib2.Latest

module Parser = Make
  (Std.Loc)
  (Std.Id)
  (Std.Term)
  (Std.Statement)

module TypeEnv = TypeEnvironment

module Term = Std.Term
module Statement = Std.Statement

open Term
open Statement

exception ParserError of string

(** Parse ssl application *)
let rec parse_app term operands = match term.term with
  | Symbol id -> begin match Format.asprintf "%a" Dolmen_std.Id.print id with
    (* n-ary connectives *)
    | "sep" -> SSL.mk_star @@ List.map parse_term operands
    | "and" -> SSL.mk_and @@ List.map parse_term operands
    | "or" -> SSL.mk_or @@ List.map parse_term operands
    (* binary connectives *)
    | "septraction" ->
      SSL.Septraction (fst @@ parse_binary_op operands, snd @@ parse_binary_op operands)
    | "wand" ->
      SSL.mk_wand (fst @@ parse_binary_op operands) (snd @@ parse_binary_op operands)
    (* unary connectives *)
    | "not" -> SSL.Not (parse_unary_op operands)
    (* binary atoms *)
    | "=" ->
      begin
        try SSL.mk_eq (fst @@ parse_atom operands) (snd @@ parse_atom operands)
        with _ -> SSL.mk_iff (fst @@ parse_binary_op operands) (snd @@ parse_binary_op operands)
      end
    | "distinct" ->
        (* TODO: variadic version *)
        SSL.mk_distinct [fst @@ parse_atom operands; snd @@ parse_atom operands]
    | "pto" -> parse_pto operands
    | "ls" -> SSL.mk_ls (fst @@ parse_atom operands) (snd @@ parse_atom operands)
    | "dll" | "dls" -> parse_dls operands
    (* TODO: fix *)
    | "skl1" -> SSL.mk_ls (fst @@ parse_atom operands) (snd @@ parse_atom operands)
    | "skl2" -> SSL.mk_skl 2 (fst @@ parse_atom operands) (snd @@ parse_atom operands)
    | other -> raise (ParserError (Format.asprintf "Unknown connective %s" other))
  end

and parse_binary_op operands =
  if List.length operands <> 2 then raise (ParserError "Expected two operands")
  else (parse_term @@ List.nth operands 0, parse_term @@ List.nth operands 1)

and parse_unary_op operands =
  if List.length operands <> 1 then raise (ParserError "Expected one operand")
  else parse_term @@ List.nth operands 0

and parse_dls operands =
  if List.length operands <> 4 then raise (ParserError "Expected one operand")
  else match List.map parse_term operands with [x; y; f; l] -> SSL.mk_dls x y f l

and parse_pto [source; targets] =
  let source_var = parse_term source in
  match targets.term with
  | App (_, args) -> SSL.mk_pto_seq source_var (List.map parse_term args)
  | _ -> SSL.mk_pto source_var (parse_term targets)

and parse_atom operands =
  if List.length operands <> 2 then raise (ParserError "Expected two variables")
  else (parse_symbol @@ List.nth operands 0, parse_symbol @@ List.nth operands 1)


and parse_constant term (id : Dolmen_std.Id.t) =
  match Format.asprintf "%a" Dolmen_std.Id.print id with
    | "emp" -> SSL.mk_emp ()
    | "sep.emp" -> SSL.mk_emp ()          (* compatibility with cvc5 *)
    | "(_ emp Loc Loc)" -> SSL.mk_emp ()  (* compatibility with SL-COMP *)
    | "(_ emp Loc\nLoc)" -> SSL.mk_emp ()  (* TODO: ... *)
    | "true" -> SSL.mk_true ()
    | "false" -> SSL.mk_false ()
    | other -> symbol_to_var term id

and parse_term term = match term.term with
  | App (t, terms) -> parse_app t terms
  | Symbol id -> parse_constant term id
  | Colon (t, _) -> parse_term t
  | _ -> failwith (Format.asprintf "Not supporter term: %a" Std.Term.print term)

and parse_smt_term t = match t.term with
  | App (fn, [x; y]) ->
      let tx = parse_smt_term x in
      let ty = parse_smt_term y in
      begin match fn.term with
        | Symbol id -> begin match Format.asprintf "%a" Dolmen_std.Id.print id with
          | "+" -> SMT.LIA.mk_plus tx ty
          | "-" -> SMT.LIA.mk_minus tx ty
          | "*" -> SMT.LIA.mk_mult tx ty
          | s -> failwith ("[LIA parser] Unknown symbol " ^ s)
        end
      end
  | Symbol id ->
    let name = Format.asprintf "%a" Dolmen_std.Id.print id in
    let re = Str.regexp "[0-9]+" in
    if Str.string_match re name 0
    then SMT.LIA.mk_const (int_of_string name)
    else match TypeEnv.type_of name with
    | Int -> SMT.LIA.mk_var name
    | Bool -> SMT.Boolean.mk_var name
    | other -> failwith (Sort.show other)

and symbol_to_var term symbol : SSL.t =
  match Format.asprintf "%a" Dolmen_std.Id.print symbol with
  | "nil" | "sep.nil" | "(sep.nil)" | "(as sep.nil Loc)" -> Var SSL.Variable.nil
  | var ->
      try
        begin match TypeEnv.type_of var with
          | Loc -> Var (SSL.Variable.mk var)
          | Int -> SSL.mk_pure_var var Sort.Int
          | Bool -> SSL.mk_pure_var var Sort.Bool
        end
      with _ -> SSL.mk_pure (parse_smt_term term)

and parse_sort id =
  match Format.asprintf "%a" Dolmen_std.Id.print id with
  | "Loc" -> (Sort.Loc)
  | "Int" -> (Sort.Int)
  | "Bool" -> (Sort.Bool)
  | other -> failwith ("Unsopported sort" ^ other)

and parse_symbol term : SSL.t = match term.term with
  (* Convert symbol to variable *)
  | Symbol symbol -> symbol_to_var term symbol
  (* Ignore type *)
  | Colon (term, typ) -> parse_symbol term
  (* Parse SSL connective *)
  | App (_, [t]) -> parse_symbol t
  (* Parse data constraints *)
  | App _ -> SSL.mk_pure (parse_smt_term term)

  | _ -> failwith (Format.asprintf "Not supporter term: %a" Std.Term.print term)

let parse_definitions defs_group =
  if defs_group.recursive then
    failwith "Recursive definitions are not supported"
  else
    let defs = defs_group.contents in
    List.fold_left
    (fun acc d -> match d with
      | Abstract a -> begin match a.ty.term with
        | Binder (_, _, t) -> begin match t.term with
          | Symbol id ->
              let sort = parse_sort id in
              let name = Format.asprintf "%a" Dolmen_std.Id.print a.id in
              TypeEnv.declare name sort;
              (* Only location variables *)
              begin match sort with
              | Loc -> symbol_to_var t a.id :: acc
              | _ -> acc
              end
          | _ -> acc
        end
      end
      | _ -> acc
  ) [] defs

(** Preprocessing on the level of the input file *)
let preprocess file =
  let channel = open_in file in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;

  (* Remove declare-heap command -- TODO: more general *)
  let re = Str.regexp "(declare-heap (Loc Loc))" in
  Str.global_replace re "" content

let parse_aux file content =
  let _, fn, _ = Parser.parse_input (`Contents (file, content)) in
  let rec unpack generator acc =
    match generator () with
    | None -> acc
    | Some x -> unpack generator (x :: acc)
  in
  List.rev @@ unpack fn []

let get_status file =
  let channel = open_in file in
  let content = really_input_string channel (in_channel_length channel) in
  let re_status = Str.regexp "(set-info :status \\([a-z]*\\))" in
  try
    let _ = Str.search_forward re_status content 0 in
    Str.matched_group 1 content
  with Not_found -> "unknown"

let parse_option term input = match term.term with
  | App (t1, [t2]) ->
    begin match Format.asprintf "%a" Term.print t1, Format.asprintf "%a" Term.print t2 with
      | (":status", "sat") -> Context.set_expected_status `Sat input
      | (":status", "unsat") -> Context.set_expected_status `Unsat input
      | (":status", "unknown") -> Context.set_expected_status `Unknown input
      | (":location-bound", n) -> Context.set_expected_loc_bound (int_of_string n) input
      | _ -> input
  end
  | _ -> input

let unpack (SSL.Var x) = x

(** Parsing *)
let parse file =
  let content = preprocess file in
  let statements = parse_aux file content in
  List.fold_left
    (fun input stmt -> match stmt.descr with
      | Set_info term -> parse_option term input
      | Antecedent term -> Context.add_assertion
        (Pure_preprocessing.preprocess @@ parse_term term) input
      | Decls defs -> Context.add_variables (List.map unpack (parse_definitions defs)) input
      | Get_model -> Context.set_get_model input
      | _ -> input
    ) Context.empty statements
