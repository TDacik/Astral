(* Parser for SMT-LIB format
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open ParserUtils

module Context = ParserContext

open Dolmen
open Dolmen_smtlib2.Latest

module Parser = Make
  (Std.Loc)
  (Std.Id)
  (Std.Term)
  (Std.Statement)

module Id = Std.Id
module Term = Std.Term
module Statement = Std.Statement

module Print = Printer.Make (struct let name = "parser" end)

open Term
open Statement

(*** ==== Generic parsing functions ==== *)

(** Parsing of symbol just prints it. *)
let parse_symbol id = Format.asprintf "%a" Id.print id

let parse_sort_id ctx sort = match parse_symbol sort with
  | "Int" -> Sort.Int
  | "Bool" -> Sort.Bool
  | "Loc" | "LS_t" | "RefSll_t" | "RefNLL_lvl1_t" -> Sort.loc_ls
  | "DLS_t" | "RefDll_t" -> Sort.loc_dls
  | "NLS_t" | "RefNLL_lvl2_t" -> Sort.loc_nls
  | other -> Sort.mk_uninterpreted other

let parse_sort ctx sort = match sort.term with
  | Binder (Arrow, [], sort) ->
    begin match sort.term with
      (* Sort of all sorts *)
      | Builtin Ttype -> raise Exit
      | Symbol id -> parse_sort_id ctx id
    end
  | Symbol id -> parse_sort_id ctx id

(*** ===== SL formula parsing ===== *)

let rec parse_term ctx term = match term.term with
  | Symbol c -> parse_constant ctx c
  | Colon (t, _) -> parse_term ctx t
  | Binder (binder, vars, body) -> parse_binder ctx binder vars body
  | App (app, operands) -> parse_application ctx app operands


(*** === Symbol parsing === *)

(* TODO: avoid hack for 'emp' *)
(* TODO: still necessary? *)
and parse_constant_hack id =
  let sub_terms = Id.split id in
  if List.mem "emp" sub_terms then "emp"
  else Format.asprintf "%a" Id.print id

and parse_constant ctx id = match parse_constant_hack id with
  | "emp" | "sep.emp" -> SSL.mk_emp ()
  | "true" -> SSL.mk_true ()
  | "false" -> SSL.mk_false ()
  | "nil" | "sep.nil" -> SSL.mk_nil ()
  | numeral when Str.string_match (Str.regexp "[0-9]+") numeral 0 ->
    SSL.mk_pure @@ SMT.Arithmetic.mk_const (int_of_string numeral)
  | var ->
    let sort = Context.type_of_var ctx var in
    match sort with
    | Loc name -> SSL.mk_var var sort
    | smt_sort -> SSL.mk_pure @@ SMT.Variable.mk var smt_sort

(*** === Binder parsing === *)

(** Currently, binder can only be existential or universal quantifier. *)
and parse_binder ctx binder vars body =
  let local_ctx, vars = parse_local_vars ctx vars in
  let body = parse_term local_ctx body in
  match binder with
    | Ex -> SSL.mk_exists vars body
    | All -> SSL.mk_forall vars body

and parse_local_vars ctx xs =
  List.fold_left
    (fun (ctx, xs) x ->
      let ctx, x' = parse_local_var ctx x in
      (ctx, x' :: xs)
    ) (ctx, []) xs

and parse_local_var ctx var = match var.term with
  (* Local variable is always typed symbol *)
  | Colon (var, sort) ->
    let name = Format.asprintf "%a" Term.print var in
    let sort = parse_sort ctx sort in
    let local_ctx = Context.declare_var ctx name sort in
    let var = match sort with
      | Loc name -> SSL.mk_var name sort
      | smt_sort -> SSL.mk_pure @@ SMT.Variable.mk name smt_sort
    in
    (local_ctx, var)


(*** === Application parsing === *)

(* TODO: simplify? *)
(* TODO: systematic pointer parsing *)
and parse_application ctx app operands = match app.term with
  | Symbol id ->
    let symbol = parse_symbol id in
    match symbol with
      | "pto" -> parse_pointer ctx operands
      | _ ->
        let operands = List.map (parse_term ctx) operands in
        begin match symbol with
        (* Variadic operators *)
        | "=" -> SSL.mk_eq_list operands
        | "distinct" -> SSL.mk_distinct_list operands
        | "sep" -> SSL.mk_star operands
        | "and" -> SSL.mk_and operands
        | "or" -> SSL.mk_or operands

        (* Unary operators *)
        | "not" -> (lift_cons SSL.mk_not "not") operands

        (* Quaternary operators *)
        | "dls" | "dll" -> (lift_cons4 SSL.mk_dls "dls") operands

        (* Binary operators *)
        | "ls" | "lso" -> (lift_cons2 SSL.mk_ls "ls") operands
        | "septraction" -> (lift_cons2 SSL.mk_septraction "septraction") operands
        | "wand" -> (lift_cons2 SSL.mk_wand "wand") operands

        (* Ternary operators *)
        | "nls" | "nll" -> (lift_cons3 SSL.mk_nls "nls") operands

        (* Arithmetic *)
        | "+" ->
          SSL.mk_pure @@ (lift_cons2 SMT.Arithmetic.mk_plus "+") (to_pure operands)
        | "-" ->
          SSL.mk_pure @@ (lift_cons2 SMT.Arithmetic.mk_minus "-") (to_pure operands)
        | "*" ->
          SSL.mk_pure @@ (lift_cons2 SMT.Arithmetic.mk_mult "*") (to_pure operands)
        | "<=" ->
          SSL.mk_pure @@ (lift_cons2 SMT.Arithmetic.mk_lesser_eq "<=") (to_pure operands)

        | other -> raise @@ ParserError ("Unknown term '" ^ other ^ "'")
    end

and parse_pointer ctx [source; target] =
  let source = parse_term ctx source in
  (* Target of pointer is either variable, or constructor *)
  match target.term with
  | App (constructor, operands) ->
    let constructor = Format.asprintf "%a" Term.print constructor in
    let operands = List.map (parse_term ctx) operands in
    begin match constructor with
    | "c_Dll_t" | "c_dls" ->
      lift_cons2 (SSL.mk_pto_dls source) "loc_dls constructor" operands
    | "c_NLL_lvl2_t" | "c_nls" ->
      lift_cons2 (SSL.mk_pto_nls source) "loc_nls constructor" operands
    | "c_Sll_t" | "c_ls" | "c_NLL_lvl1_t" ->
      lift_cons (SSL.mk_pto source) "loc_ls constructor" operands
    | other -> raise @@ ParserError ("Unknown constructor '" ^ other ^ "'")
    end
  | _ -> SSL.mk_pto source (parse_term ctx target)

and to_pure xs =
  List.map
    (fun ssl -> match ssl with
      | SSL.Var (x, Sort.Int) -> SMT.Arithmetic.mk_var x
      | SSL.Var (x, Sort.Bool) -> SMT.Boolean.mk_var x
      | SSL.Pure phi -> phi
      | SSL.Eq xs -> SMT.mk_eq_list @@ to_pure xs
      | SSL.Distinct xs -> SMT.mk_distinct_list @@ to_pure xs
      | SSL.And (psi1, psi2) -> SMT.Boolean.mk_and @@ to_pure [psi1; psi2]
      | SSL.Or (psi1, psi2) -> SMT.Boolean.mk_or @@ to_pure [psi1; psi2]
    ) xs

(*** ==== Parsing of top-level constructions ==== *)

(** Parse single declaration *)
let parse_declaration ctx = function
  | Abstract decl ->
    (* Variable declaration *)
    try
      let name = parse_symbol decl.id in
      let sort = parse_sort ctx decl.ty in
      Context.declare_var ctx name sort

    (* Declaration of uninterpreted sort *)
    with Exit ->
      let sort = parse_sort_id ctx decl.id in
      Context.declare_sort ctx sort

(** Parse group of declarations *)
let parse_declarations ctx decls =
  (* TODO: parsing of user-defined inductive predicates *)
  if decls.recursive then ctx
  else List.fold_left parse_declaration ctx decls.contents

let parse_option ctx opt = match opt.term with
  | App (name, [value]) ->
    let name = Format.asprintf "%a" Term.print name in
    let value = Format.asprintf "%a" Term.print value in
    begin match name with
      | ":status" -> Context.set_expected_status ctx value
      | _ -> Context.set_attribute ctx name value
    end

let parse_assertion ctx term =
  parse_term ctx term
  |> Normalisation.apply
  |> Context.add_assertion ctx

let parse_statements content =
  let _, fn, _ = Parser.parse_input (`Contents ("", content)) in
  let rec unpack generator acc =
    match generator () with
    | None -> acc
    | Some x -> unpack generator (x :: acc)
  in
  List.rev @@ unpack fn []

let parse ctx content =
  let statements = parse_statements content in
  List.fold_left
    (fun ctx stmt -> match stmt.descr with
      | Set_info term -> parse_option ctx term
      | Decls defs -> parse_declarations ctx defs
      | Antecedent phi -> parse_assertion ctx phi
      | Get_model -> Context.set_produce_models ctx true
      | _ -> ctx
    ) ctx statements

let parse ?(ctx=Context.empty) content =
  try parse ctx content
  with ParserError msg -> failwith msg
