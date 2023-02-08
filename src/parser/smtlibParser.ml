(* Parser for SMT-LIB format
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

module Id = Std.Id
module Term = Std.Term
module Statement = Std.Statement

module Print = Printer.Make (struct let name = "parser" end)

open Term
open Statement

exception ParserError of string

let parse_symbol id = Format.asprintf "%a" Id.print id

let parse_sort sort = match sort.term with
  | Binder (Arrow, [], sort) ->
    begin match sort.term with
      | Builtin Ttype -> raise Exit (* Sort of all sorts *)
      | Symbol id ->
        begin match parse_symbol id with
          | "Loc" -> Sort.Loc
          | "Int" -> Sort.Int
          | "Bool" -> Sort.Bool
        end
    end
  | Symbol id ->
    begin match parse_symbol id with
      | "Loc" -> Sort.Loc
      | "Int" -> Sort.Int
      | "Bool" -> Sort.Bool
    end


let parse_option context opt = match opt.term with
  | App (t1, [t2]) ->
    let option_name = Format.asprintf "%a" Term.print t1 in
    let option_value = Format.asprintf "%a" Term.print t2 in
    begin match option_name, option_value with
      | (":status", "sat") -> Context.set_expected_status `Sat context
      | (":status", "unsat") -> Context.set_expected_status `Unsat context
      | (":status", "unknown") -> Context.set_expected_status `Unknown context
      | (":location-bound", n) -> Context.set_expected_loc_bound (int_of_string n) context
      | _ -> context
    end

let parse_declaration context type_env = function
  | Abstract decl ->
    try
      let name = parse_symbol decl.id in
      let sort = parse_sort decl.ty in
      let type_env' = TypeEnv.declare type_env name sort in
      let context' = Context.add_variables [name, sort] context in
      (context', type_env')

    (* TODO: Sort declaration *)
    with Exit -> context, type_env

let parse_declarations context type_env decls =
  (* TODO: parsing of user-defined inductive predicates *)
  if decls.recursive then (context, type_env)
  else List.fold_left
    (fun (context, type_env) decl ->
      parse_declaration context type_env decl
    ) (context, type_env) decls.contents

(** Formula parsing *)

let raise_incorrect_arity name n =
  let msg = Format.asprintf "Operator %s expects %d operands" name n in
  raise @@ ParserError msg

let lift_unop_to_list fn operator = function
    | [x1] -> fn x1
    | _ -> raise_incorrect_arity operator 2

let lift_binop_to_list fn operator = function
    | [x1; x2] -> fn x1 x2
    | _ -> raise_incorrect_arity operator 2

let lift_quadop_to_list fn operator = function
    | [x1; x2; x3; x4] -> fn x1 x2 x3 x4
    | _ -> raise_incorrect_arity operator 4

let rec parse_assertion context type_env phi =
  let phi = parse_term type_env phi in
  let phi = Normalisation.apply phi in
  Context.add_assertion phi context

and parse_term type_env term = match term.term with
  | Symbol c -> parse_constant type_env c
  | Colon (t, _) -> parse_term type_env t (* TODO: do we need to check types anywhere? *)
  | App (f, operands) -> parse_application type_env f operands
  | Binder (kind, xs, psi) ->
    let type_env', xs = parse_binders type_env xs in
    let psi = parse_term type_env' psi in
    begin match kind with
    | Ex -> SSL.mk_exists xs psi
    | All -> SSL.mk_forall xs psi
    end

and parse_constant type_env id = match parse_symbol id with
  | "emp" | "sep.emp" -> SSL.mk_emp ()
  | "true" -> SSL.mk_true ()
  | "false" -> SSL.mk_false ()
  | "nil" | "sep.nil" -> SSL.mk_nil ()
  | numeral when Str.string_match (Str.regexp "[0-9]+") numeral 0 ->
      SSL.mk_pure @@ SMT.Arithmetic.mk_const (int_of_string numeral)
  | var ->
      let sort = TypeEnv.type_of type_env var in
      match sort with
      | Loc -> SSL.mk_var var
      | smt_sort -> SSL.mk_pure @@ SMT.Variable.mk var smt_sort

and parse_binder type_env binder = match binder.term with
  | Colon (var, sort) ->
    let name = parse_var_name var in
    let sort = parse_sort sort in
    let type_env' = TypeEnv.declare type_env name sort in
    (type_env', SSL.mk_var_sort name sort)

and parse_binders type_env binders =
  List.fold_left
    (fun (type_env, xs) x ->
      let type_env', x' = parse_binder type_env x in
      (type_env', x' :: xs)
    ) (type_env, []) binders

and parse_var_name var = match var.term with
  | Symbol id -> parse_symbol id

and parse_smt xs =
  let rec convert = function
    | SSL.Var (x, Sort.Int) -> SMT.Arithmetic.mk_var x
    | SSL.Var (x, Sort.Bool) -> SMT.Boolean.mk_var x
    | SSL.Pure phi -> phi
    | SSL.Eq xs -> SMT.mk_eq_list (List.map convert xs)
    | SSL.Distinct xs -> SMT.mk_distinct_list (List.map convert xs)
    | SSL.And (psi1, psi2) -> SMT.Boolean.mk_and [convert psi1; convert psi2]
    | SSL.Or (psi1, psi2) -> SMT.Boolean.mk_or [convert psi1; convert psi2]
  in
  List.map convert xs

and parse_application type_env app operands = match app.term with
  | Symbol id ->
    let symbol = parse_symbol id in
    match symbol with
      | "pto" -> parse_pointer type_env operands
      | _ ->
        let operands = List.map (parse_term type_env) operands in
        begin match parse_symbol id with
        (* Variadic operators *)
        | "=" -> SSL.mk_eq_list operands
        | "distinct" -> SSL.mk_distinct_list operands
        | "sep" -> SSL.mk_star operands
        | "and" -> SSL.mk_and operands
        | "or" -> SSL.mk_or operands

        (* Unary operators *)
        | "not" -> (lift_unop_to_list SSL.mk_not "not") operands

        (* Quaternary operators *)
        | "dls" | "dll" -> (lift_quadop_to_list SSL.mk_dls "dls") operands

        (* Binary operators *)
        | "ls" -> (lift_binop_to_list SSL.mk_ls "ls") operands
        | "septraction" -> (lift_binop_to_list SSL.mk_septraction "septraction") operands
        | "wand" -> (lift_binop_to_list SSL.mk_wand "wand") operands

        (* Arithmetic *)
        | "+" ->
          SSL.mk_pure @@ (lift_binop_to_list SMT.Arithmetic.mk_plus "+") (parse_smt operands)
        | "-" ->
          SSL.mk_pure @@ (lift_binop_to_list SMT.Arithmetic.mk_minus "-") (parse_smt operands)
        | "*" ->
          SSL.mk_pure @@ (lift_binop_to_list SMT.Arithmetic.mk_mult "*") (parse_smt operands)
        | "<=" ->
          SSL.mk_pure @@ (lift_binop_to_list SMT.Arithmetic.mk_lesser_eq "<=") (parse_smt operands)

    end

and parse_pointer type_env [source; target] =
  let source_var = parse_term type_env source in
  match target.term with
  | App (_, args) -> SSL.mk_pto_seq source_var (List.map (parse_term type_env) args)
  | _ -> SSL.mk_pto source_var (parse_term type_env target)

let parse_statements content =
  let _, fn, _ = Parser.parse_input (`Contents ("", content)) in
  let rec unpack generator acc =
    match generator () with
    | None -> acc
    | Some x -> unpack generator (x :: acc)
  in
  List.rev @@ unpack fn []

let parse context content =
  let statements = parse_statements content in
  let context, type_env = List.fold_left
    (fun (context, type_env) stmt -> match stmt.descr with
      | Set_info term -> parse_option context term, type_env
      | Decls defs -> parse_declarations context type_env defs
      | Antecedent phi -> parse_assertion context type_env phi, type_env
      | Get_model -> Context.set_get_model context, type_env
      | _ -> context, type_env
    ) (context, TypeEnv.empty) statements
  in
  {context with phi = SSL.mk_and context.phi_orig}

let parse context content =
  try parse context content
  with ParserError msg -> failwith msg
