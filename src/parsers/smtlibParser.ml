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

open Std.Term
open Std.Statement

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
    | "=" -> SSL.Eq (fst @@ parse_atom operands, snd @@ parse_atom operands)
    | "distinct" ->
        SSL.Neq (fst @@ parse_atom operands, snd @@ parse_atom operands) (* TODO: n-ary distinct *)
    | "pto" -> SSL.PointsTo (fst @@ parse_atom operands, snd @@ parse_atom operands)
    | "ls" -> SSL.LS (fst @@ parse_atom operands, snd @@ parse_atom operands)
    | other -> raise (ParserError (Format.asprintf "Unknown connective %s" other))
  end

and parse_binary_op operands =
  if List.length operands <> 2 then raise (ParserError "Expected two operands")
  else (parse_term @@ List.nth operands 0, parse_term @@ List.nth operands 1)

and parse_unary_op operands =
  if List.length operands <> 1 then raise (ParserError "Expected one operand")
  else parse_term @@ List.nth operands 0

and parse_atom operands =
  if List.length operands <> 2 then raise (ParserError "Expected two variables")
  else (parse_symbol @@ List.nth operands 0, parse_symbol @@ List.nth operands 1)

and parse_constant (id : Dolmen_std.Id.t) =
  match Format.asprintf "%a" Dolmen_std.Id.print id with
    | "emp" -> SSL.mk_emp ()
    | "sep.emp" -> SSL.mk_emp ()          (* compatibility with cvc5 *)
    | "(_ emp Loc Loc)" -> SSL.mk_emp ()  (* compatibility with SL-COMP *)
    | "true" -> SSL.mk_true ()
    | "false" -> SSL.mk_false ()
    | other -> raise (ParserError (Format.asprintf "Unkown constant: %s" other))

and parse_term term = match term.term with
  | App (t, terms) -> parse_app t terms
  | Symbol id -> parse_constant id
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
    else match name with
    | _ -> SMT.LIA.mk_var name

and symbol_to_var term symbol =
  match Format.asprintf "%a" Dolmen_std.Id.print symbol with
  | "nil" -> SSL.Variable.Nil
  | var ->
      try
        begin match TypeEnv.type_of var with
          | Loc -> SSL.Variable.mk var
          | Int -> SSL.Variable.Term (SMT.LIA.mk_var var)
        end
      with _ -> SSL.Variable.Term (parse_smt_term term)

and parse_sort id =
  match Format.asprintf "%a" Dolmen_std.Id.print id with
  | "Loc" -> SSL.Sort.Loc
  | "Int" -> SSL.Sort.Int
  | "Bool" -> SSL.Sort.Bool
  | other -> failwith ("Unsopported sort" ^ other)

and parse_symbol term = match term.term with
  (* Convert symbol to variable *)
  | Symbol symbol -> symbol_to_var term symbol
  (* Ignore type *)
  | Colon (term, typ) -> parse_symbol term
  (* Parse SSL connective *)
  | App (_, [t]) -> parse_symbol t
  (* Parse data constraints *)
  | App _ -> SSL.Variable.Term (parse_smt_term term)

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
              symbol_to_var t a.id :: acc
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

(** Parsing *)
let parse file =
  let content = preprocess file in
  let statements = parse_aux file content in
  let assertions, vars = List.fold_left
    (fun (assertions, vars) stmt -> match stmt.descr with
      | Antecedent term -> ((parse_term term) :: assertions, vars)
      | Decls def_group -> (assertions, vars @ parse_definitions def_group)
      | _ -> (assertions, vars)
    ) ([], []) statements
  in
  let phi = SSL.mk_and assertions in
  (phi, vars)
