(* Parser for (a fragment of) separation logic encoded in smt-lib format
 * (see https://sl-comp.github.io/docs/smtlib-sl.pdf).
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2021 *)

open ParserUtils
open MemoryModel

module Context = ParserContext

open Dolmen_std
open Dolmen_smtlib2.Script.Latest
open Term
open Statement

module Extension = struct
  (* TODO: fail for non declare-heap command *)
  let statement str = Some (fun ?(loc=Loc.no_loc) terms ->
    let name = Id.create Id.decl (Name.simple "declare-heap") in
    {
      id = None;
      descr = Other {name = name; args = terms};
      attrs = [];
      loc = loc;
    }
  )
end

module Parser = Make(Loc)(Id)(Term)(Statement)(Extension)

module Logger = Logger.Make (struct let name = "parser" let level = 3 end)

(** Parser AST *)

type parser_ast =
  | SMT of SMT.t
  | Term of SL.Term.t
  | Formula of SL.t

let show_aux typ ast =
  Format.asprintf "%s (%s%s%s)" ast Colors.blue typ Colors.white

let show = function
  | Formula phi -> show_aux "SL formula" (SL.show_with_sort phi)
  | Term t -> show_aux "SL term" (SL.Term.show_with_sort t)
  | SMT smt -> show_aux "SMT term" (SMT.show_with_sort smt)

let show_list asts = String.concat "\n  " @@ List.map show asts

(** AST coercisions *)

let get_smt ?(loc=Loc.no_loc) name = function
  | SMT term -> term
  (*| Term t when SL.Term.is_nil t -> Obj.magic t (* TODO *)*)
  | other -> raise @@ SortError (loc, name, "SMT term", show other)

let get_term ?(loc=Loc.no_loc) name = function
  | SMT smt -> SL.Term.mk_smt smt
  | Term term -> term
  | other -> raise @@ SortError (loc, name, "SL term", show other)

let get_formula ?(loc=Loc.no_loc) ?(name="TODO") = function
  | SMT smt when SMT.has_sort Sort.bool smt -> SL.mk_pure smt
  | Formula phi -> phi
  | other -> raise @@ SortError (loc, name, "SL formula", show other)

let mk_formula fn args = Formula (fn @@ List.map get_formula args)
let mk_atom name fn args = Formula (fn @@ List.map (get_term name) args)
let mk_term fn args = Term (fn @@ List.map get_term args)
let mk_smt name fn args = SMT (fn @@ List.map (get_smt name) args)

(*** ==== Identifiers ==== *)

(** Parsing of symbol just prints it. *)
let parse_id id = Format.asprintf "%a" Id.print id

(*** ==== Sorts ==== *)

let parse_sort_name ctx = function
  | "Int" -> Sort.int
  | "Bool" -> Sort.bool
  | bv when Str.string_match (Str.regexp {|(_ BitVec \([0-9]+\))|}) bv 0 ->
    Sort.mk_bitvector @@ int_of_string @@ Str.matched_group 1 bv
  | other -> Context.find_sort ctx other

let parse_sort_id ctx sort = parse_sort_name ctx @@ parse_id sort

(** Simple sorts are represented as () -> Sort *)
let parse_sort ctx sort = match sort.term with
  | Binder (Arrow, [], sort) ->
    begin match sort.term with
      (* Sort of all sorts *)
      | Builtin Ttype ->
        Logger.debug "  Parsing sort %a (metasort)\n" Term.print sort;
        raise Exit
      | Symbol id -> parse_sort_id ctx id
    end
  | Symbol id ->
    Logger.debug "  Parsing sort %a\n" Term.print sort;
    parse_sort_id ctx id
  | _ -> raise @@ NotSupported (sort.loc, Format.asprintf "sort: %a" Term.print sort)

(*** ===== SL formula parsing ===== *)

let rec parse_formula ctx (phi : Term.t) = match parse_term ctx phi with
  | Formula phi -> phi
  | SMT pure -> SL.mk_pure pure
  | Term term ->
    raise @@ SortError (phi.loc, "Assertion", "Bool", Sort.show @@ SL.Term.get_sort term)

and parse_term ctx (term : Term.t) =
  Logger.debug "  Parsing term %a\n" Term.print term;
  match term.term with
  | Symbol c -> parse_constant_term ctx term c
  | Colon (t, _) -> parse_term ctx t
  | Binder (binder, vars, body) -> parse_binder ctx term binder vars body
  | App (app, operands) -> parse_application ctx term app operands

and parse_constant_term ctx term id =
  Logger.debug "  Parsing constant %a " Id.print id;
  match parse_id id with
  | "true" -> Formula SL.tt
  | "false" -> Formula SL.ff
  | "emp" | "sep.emp" -> Formula SL.emp
  | emp when String.starts_with ~prefix:"(_ emp" emp -> Formula SL.emp
  | "nil" | "sep.nil" -> Term SL.Term.nil
  | numeral when Str.string_match (Str.regexp "[0-9]+") numeral 0 ->
    SMT (SMT.Arithmetic.mk_const (int_of_string numeral))
  | bv_hex when Str.string_match (Str.regexp "0x([0-9]|[A-F])+") bv_hex 0 ->
    SMT (SMT.Bitvector.mk_const_of_string bv_hex)
  | var ->
    let sort = Context.type_of_var ctx var in
    if Sort.is_loc sort (* TODO *)
    then Term (SL.Term.mk_var var sort)
    else SMT (SMT.mk_var var sort)

(** Currently, binder can only be existential or universal quantifier. *)
and parse_binder ctx term binder vars body =
  let local_ctx, vars = parse_local_vars ctx vars in
  let body = parse_formula local_ctx body in
  match binder with
    | Ex -> Formula (SL.mk_exists vars body)
    | All -> Formula (SL.mk_forall vars body)
    | _ -> raise @@ NotSupported (term.loc, "binder different than forall/exists")

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
      | Loc _ -> SL.Variable.mk name sort
      | smt_sort -> SL.Variable.mk name smt_sort
    in
    (local_ctx, var)


(*** === Application parsing === *)

and sort_of_list args =
  (*if List.mem (Term (SL.Term.nil)) args then Term SL.Term.nil
  else*)
  try List.find (function Term t -> true | _ -> false) args
  with Not_found -> List.hd args

(* TODO: check same sort *)
and parse_eq ctx args = match sort_of_list args with
  | Formula _ -> mk_formula SL.mk_iff args
  | Term _ -> mk_atom "=" SL.mk_eq args
  | SMT smt -> mk_smt "=" SMT.mk_eq args

(* TODO: check same sort *)
and parse_distinct ctx args = match sort_of_list args with
  | Formula _ -> mk_formula (fun xs -> SL.mk_not @@ SL.mk_iff xs) args
  | Term _ -> mk_atom "distinct" SL.mk_distinct args
  | SMT _ -> mk_smt "distinct" SMT.mk_distinct args

(** Pointer can be in one of the following forms:
      - simple pointer (pto x y) where y is variables
      - smt-pointer (pto x term)
      - pointer to structure (pto x (constructor y1 ... yn))

    The cases of smt-pointer and pointer to structure currently cannot be
    easily distuingished. *)
and parse_pointer ctx [source; target] =
  Logger.debug "  Parsing pointer %a -> %a \n" Term.print source Term.print target;
  let source = parse_term ctx source in
  let res = match target.term with
  (* Pointer to a structure or smt term *)
  | App (app, operands) ->
    let app_name = Format.asprintf "%a" Term.print app in
    if Context.is_declared_struct ctx app_name then
      let operands = List.map (parse_term ctx) operands in
      let def = Context.find_struct_def_by_cons ctx app_name in
      SL.mk_pto_struct (get_term "pto" source) def (List.map (get_term "struct") operands)
    else
      let source = get_term "pointer source" source in
      let target = get_term "pointer target" @@ parse_term ctx target in
      SL.mk_pto source target

  (* Simple pointer *)
  | _ ->
    let source = get_term "pointer source" source in
    let target = get_term "pointer target" @@ parse_term ctx target in
    SL.mk_pto source target
  in
  Formula res

and parse_predicate (ctx : Context.t) pred operands =
  let operands = List.map (get_term pred) operands in
  if Context.is_declared_pred ctx pred && not @@ SID.is_builtin pred then
    Formula (SL.mk_predicate pred operands)
  else match SID.instantiate ctx.heap_sort pred operands with
    | Ok phi -> Formula phi
    | Error msg -> failwith msg

(* TODO: arity *)
and parse_connective1 name ctx sl_cons smt_cons = function
  | [arg] -> begin match arg with
    | Formula arg -> Formula (sl_cons arg)
    | SMT arg -> SMT (smt_cons arg)
    end

and parse_connective_list name ctx sl_cons smt_cons args = match List.hd args with
  | Formula _ -> mk_formula sl_cons args
  | SMT _ -> mk_smt name smt_cons args

and parse_smt1 name cons = function [SMT arg] -> SMT (cons arg)
and parse_smt2 name cons = function [SMT arg1; SMT arg2] -> SMT (cons arg1 arg2)
and parse_smt3 name cons = function [SMT arg1; SMT arg2; SMT arg3] -> SMT (cons arg1 arg2 arg3)

and parse_smt_list name cons args = match List.hd args with
  | SMT _ -> mk_smt name cons args
  | Term t -> failwith (Format.asprintf "Unexpected SL term '%s'" (SL.Term.show t))

and parse_sl_term1 name cons = function
  | [Term arg] -> Term (cons arg)
  | [SMT arg] -> Term (cons @@ SL.Term.mk_smt arg)
  | [Formula arg] -> failwith @@ Format.asprintf "%s expected SL term, got SL formula %s" name (SL.show arg)
  | xs -> failwith @@ Format.asprintf "%s xpected 1 argument, got %d" name (List.length xs)

and parse_sl2 name cons = function [Formula psi1; Formula psi2] -> Formula (cons psi1 psi2)

and parse_sl_list loc name cons args = mk_formula cons args

and parse_application ctx term app operands =
  Logger.debug "  Parsing application %a\n" Term.print app;
  match app.term with
  | Symbol id -> begin match parse_id id with
    | "pto" -> parse_pointer ctx operands
    | _ -> parse_non_pointer_application ctx term app operands
  end

and parse_non_pointer_application ctx term app operands =
  let loc = term.loc in
  let args = List.map (parse_term ctx) operands in
  match app.term with
  | Symbol id -> begin match parse_id id with
    (* Ambiguos atoms *)
    | "=" -> parse_eq ctx args
    | "distinct" -> parse_distinct ctx args

    (* Separation logic atoms *)
    | pred when Context.is_declared_pred ctx pred -> parse_predicate ctx pred args

    (* Ambigous connectives *)
    | "and" -> parse_connective_list "and" ctx SL.mk_and SMT.Boolean.mk_and args
    | "or" -> parse_connective_list "or" ctx SL.mk_or SMT.Boolean.mk_or args
    | "not" -> parse_connective1 "not" ctx SL.mk_not SMT.Boolean.mk_not args

    (* Separation logic terms *)
    | "begin" -> parse_sl_term1 "begin" SL.Term.mk_block_begin args
    | "end" -> parse_sl_term1 "end" SL.Term.mk_block_end args

    (* Separation logic connectives *)
    | "sep" -> parse_sl_list loc "sep" SL.mk_star args
    | "septraction" -> parse_sl2 "septraction" SL.mk_septraction args
    | "wand" -> parse_sl2 "wand" SL.mk_wand args

    (* SMT terms *)
    | "+" -> parse_smt_list "+" SMT.Arithmetic.mk_plus args
    | "*" -> parse_smt_list "*" SMT.Arithmetic.mk_mult args
    | "-" -> parse_smt2 "-" SMT.Arithmetic.mk_minus args

    (*
    | "bvnot" -> parse_smt1 "bvnot" SMT.Bitvector.mk_not args
    | "bvand" -> lift_bitvector_list "bvand" SMT.Bitvector.mk_and args
    | "bvor"  -> lift_bitvector_list "bvor"  SMT.Bitvector.mk_or args
    | "bvadd" -> lift_bitvector_list "bvadd" SMT.Bitvector.mk_plus args
    *)
    | other ->
      Logger.debug "%s\n" (Context.show ctx);
      raise @@ ParserError ("Unknown application '" ^ other ^ "'")

    end

and parse_smt_constant_or_var ctx symbol = match parse_id symbol with
  | numeral when Str.string_match (Str.regexp "[0-9]+") numeral 0 ->
    SMT.Arithmetic.mk_const (int_of_string numeral)
  | var ->
    let sort = Context.type_of_var ctx var in
    SMT.mk_var var sort

(*** ==== Parsing of top-level constructions ==== *)

(** Parse single declaration *)
let parse_declaration ctx decl =
  Logger.debug "Parsing declaration %a\n" Statement.print_decl decl;
  match decl with
  | Abstract decl ->
    (* Variable declaration *)
    try
      let name = parse_id decl.id in
      let sort = parse_sort ctx decl.ty in
      Context.declare_var ctx name sort

    (* Declaration of uninterpreted sort *)
    with Exit ->
      let sort = Format.asprintf "%a" Id.print decl.id in
      Logger.debug "Declaring location sort %s" sort;
      let sort = Sort.mk_loc sort in
      Context.declare_sort ctx sort

let parse_field_name name = match name.term with
  | Symbol id -> parse_id id

let parse_field ctx (field : Term.t) = match field.term with
  | Colon (name, sort) ->
    let field = parse_field_name name in
    let sort = parse_sort ctx sort in
    Field.mk field sort

let parse_datatype ctx = function
  | Inductive decl ->
    let name = parse_id decl.id in
    let cons, terms = List.hd decl.cstrs in (* Structures always have single constructor *)
    let cons = parse_id cons in
    let fields = List.map (parse_field ctx) terms in
    Context.declare_struct ctx name cons fields

(** Parse group of declarations *)
let parse_declarations ctx decls =
  if decls.recursive
  (* Datatypes *)
  then List.fold_left parse_datatype ctx decls.contents
  (* Other declarations *)
  else List.fold_left parse_declaration ctx decls.contents

let parse_predicate_type ctx vars =
  List.fold_left (fun (ctx, acc) var ->
    let ctx', var = parse_local_var ctx var in
    ctx', acc @ [var]
  ) (ctx, []) vars

let parse_predicate ctx (def : def) =
  Logger.debug "Parsing inductive predicate %a\n" Term.print def.body;
  let name = parse_id def.id in
  let local_ctx, header = parse_predicate_type ctx def.params in

  Logger.debug "Header: %s\n" (SL.Variable.show_list header);
  let body = parse_formula local_ctx def.body in
  SID.add name header body;
  ctx

let parse_definition ctx (defs : def group) =
  Logger.debug "Parsing definition group\n";
  if defs.recursive then
    let names = List.map (fun (def : def) -> parse_id def.id) defs.contents in
    let ctx = List.fold_left Context.declare_pred ctx names in
    List.fold_left parse_predicate ctx defs.contents
  else failwith "Unsupported definition"

let parse_info ctx opt = match opt.term with
  | App (name, [value]) ->
    let name = Format.asprintf "%a" Term.print name in
    let value = Format.asprintf "%a" Term.print value in
    begin match name with
      | ":status" -> Context.set_expected_status ctx value
      | _ -> Context.set_attribute ctx name value
    end

let parse_option ctx opt = match opt.term with
  | Symbol id -> begin match Format.asprintf "%a" Id.print id with
    | ":use-builtin-definitions" -> SID.builtin_context () (* TODO: preserve ctx *)
    | _ -> ctx
    end
  | _ -> ctx

let parse_assertion ctx term =
  parse_formula ctx term
  (*|> Normalisation.apply*)
  |> Context.add_assertion ctx

type t =
  | Atom of string
  | List of Term.t list

let unpack_sexp sexp = match sexp.term with
  | App (sexp, body) -> begin match sexp.term with
    | Builtin (Sexpr) -> List body
    end
  | _ -> Atom (Format.asprintf "%a" Term.print sexp)

let parse_sexp_as_sort ctx sexp = match unpack_sexp sexp with
  | Atom term -> parse_sort_name ctx term
  | List terms ->
    terms
    |> List.map (fun t -> Format.asprintf "%a" Term.print t)
    |> String.concat " "
    |> (fun s -> "(" ^ s ^ ")")
    |> parse_sort_name ctx

let parse_heap_sort ctx sexp =
  let (List body) = unpack_sexp sexp in
  let dom = parse_sexp_as_sort ctx @@ List.nth body 0 in
  let range =
    match unpack_sexp @@ List.nth body 1 with
    | Atom name ->
      begin try Context.find_struct_def_by_name ctx name
      with ParserUtils.StructNotDeclared _ ->
        MemoryModel.StructDef.lift_sort (parse_sort_name ctx name)
      end
    | List terms ->
      let sort =  parse_sexp_as_sort ctx @@ (List.nth body 1) in
      MemoryModel.StructDef.lift_sort sort
  in
  Logger.debug "Heap sort element: (%s %s)\n" (Sort.show dom) (StructDef.show range);
  (dom, range)

let parse_extension ctx extension = match parse_id extension.name with
  | "declare-heap" ->
    let mapping = List.map (parse_heap_sort ctx) extension.args in
    Logger.debug "Heap sort: %s\n" (HeapSort.show @@ HeapSort.of_list mapping);
    Context.declare_heap_sort ctx mapping

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
    (fun ctx stmt ->
      Logger.debug "Parsing statement %a\n" Statement.print stmt;
      match stmt.descr with
      | Set_info term -> parse_info ctx term
      | Set_option term -> parse_option ctx term
      | Decls decls -> parse_declarations ctx decls
      | Other extension -> parse_extension ctx extension
      | Defs defs -> parse_definition ctx defs
      | Antecedent phi -> parse_assertion ctx phi
      | Get_model -> Context.set_produce_models ctx true
      | _ ->
        Logger.debug "Ignoring statement %a\n" Statement.print stmt;
        ctx
    ) ctx statements

let parse content =
  let ctx = Context.empty () in
  try parse ctx content
  with ParserError msg -> failwith msg

let parse_string ?(filename="") str =
  try parse str
  with
    | Dolmen_std.Loc.Syntax_error (position, msg) ->
      let file = Dolmen_std.Loc.mk_file filename in
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

let parse_file path =
  let channel = In_channel.open_text path in
  let content = In_channel.input_all channel in
  In_channel.close channel;
  parse_string ~filename:path content
