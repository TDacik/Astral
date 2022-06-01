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

open Dolmen_std.Id

exception ParserError of string

(** Parse ssl application *)
let rec parse_app term operands = match term.term with
  | Symbol id -> begin match id.name with
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

(** TODO: do this properly *)
and parse_constant (id : Dolmen_std.Id.t) =
    let name = Format.asprintf "%a" Dolmen_std.Id.print id in
    if name = "emp"                   then (SSL.mk_emp ())
    else if name = "(_ emp Loc Loc)"  then (SSL.mk_emp ())
    else if name = "true"             then (SSL.mk_true ())
    else if name = "false"            then (SSL.mk_false ())
    else raise (ParserError (Format.asprintf "Unkown constant: %s" name))

and parse_term term = match term.term with
  | App (t, terms) -> parse_app t terms
  | Symbol id -> parse_constant id
  | _ -> failwith (Format.asprintf "Not supporter term: %a" Std.Term.print term)

and symbol_to_var term symbol = match symbol.name with
  | "nil" -> SSL.Variable.Nil
  | var ->
      try
        begin match TypeEnv.type_of var with
          | Loc -> SSL.Variable.mk var
          | Int -> SSL.Variable.Term (LIA.Var var)
        end
      with _ -> SSL.Variable.Term (LIA.parse term)

and parse_sort id = match id.name with
  | "Loc" -> SSL.Sort.Loc
  | "Int" -> SSL.Sort.Int
  | "Bool" -> SSL.Sort.Bool
  | _ -> failwith ("Unsopported sort" ^ id.name)

and parse_symbol term = match term.term with
  (* Convert symbol to variable *)
  | Symbol symbol -> symbol_to_var term symbol
  (* Ignore type *)
  | Colon (term, typ) -> parse_symbol term
  (* Parse SSL connective *)
  | App (_, [t]) -> parse_symbol t
  (* Parse data constraints *)
  | App _ -> SSL.Variable.Term (LIA.parse term)

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
              TypeEnv.declare a.id.name sort;
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
