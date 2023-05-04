(* Parsing of input related to separation logic.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Dolmen.Std.Term
open Dolmen.Std.Statement

open Context
open TypeEnvironment

(** Auxilliary declaration of sorts needed to parse heap sort. *)
let aux_decl type_env =
  Sort.Set.elements type_env.sorts
  |> List.map (fun s -> Format.asprintf "(declare-sort %a 0)" Sort.pp s)
  |> String.concat "\n"

let check_sort name sort = match sort with
  | Sort.Loc | Sort.Uninterpreted _ -> ()
  | other ->
      failwith (Format.asprintf "%s must be uninterpreted sort, not %s\n" name (Sort.show sort))

let parse_sort sort = match sort.term with
  | Symbol _ ->
    begin match Format.asprintf "%a" Dolmen_std.Term.print sort with
      | "Loc" -> Sort.Loc
      | other -> Sort.mk_uninterpreted other
    end

let parse_heap_sort sort = match sort.term with
  | App (dom, [range]) ->
    begin
      let dom = parse_sort dom in
      let range = parse_sort range in
      let orig = Sort.mk_array dom range in
      let updated = Sort.mk_array Sort.Loc (Sort.substitute range dom Sort.Loc) in
      check_sort "Heap domain" dom;
      check_sort "Heap range" range;
      (dom, orig, updated)
    end

  (* skip *)
  | Builtin Ttype -> raise Exit

let parse_heap group =
  match List.hd group.contents with
  | Abstract def ->
    begin match def.ty.term with
      | Binder (Arrow, [], sort) -> parse_heap_sort sort
    end

let preprocess_loc_vars context =
  let dom = context.type_env.loc_sort in
  let loc_vars, smt_vars =
   List.partition_map
    (fun (name, sort) ->
      let var' = (name, Sort.substitute sort dom Sort.Loc) in
      match SSL.Variable.get_sort var' with
        | Sort.Loc -> Left var'
        | _ -> Right var'
    ) (SSL.Variable.nil :: context.smt_vars)
  in
  let phi = List.map (fun psi -> SSL.map
    (function
      | Var (name, sort) -> Var (name, Sort.substitute sort dom Sort.Loc)
      | Pure (SMT.Variable (name, sort)) ->
        if Sort.equal sort dom then Var (name, Sort.Loc)
        else Pure (SMT.Variable (name, sort))
      | phi -> phi
    ) psi) context.phi_orig in
  {context with
    smt_vars = smt_vars;
    vars_orig = context.vars_orig @ loc_vars;
    phi_orig = phi;
    phi = SSL.mk_and phi;
    vars = context.vars_orig @ loc_vars;
  }

(** Parse declare-heap command. *)
let parse type_env heap_decl =
  let input = Format.asprintf "%s\n(declare-const heap %s)\n\n" (aux_decl type_env) heap_decl in
  let stmts = SmtlibParser.parse_statements input in
  let dom, heap_type_orig, heap_type = List.fold_left
    (fun acc stmt -> match stmt.descr with
      | Decls defs ->
        try parse_heap defs
        with Exit -> acc
      | _ -> acc
    ) (Sort.Loc, type_env.heap_sort, type_env.heap_sort) stmts
  in
  TypeEnvironment.declare_heap_sort heap_type type_env
  |> TypeEnvironment.declare_loc_sort dom
