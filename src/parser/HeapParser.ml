(* Parser for declare-heap command.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module Context = ParserContext
open Context

open Dolmen.Std.Term
open Dolmen.Std.Statement

(** To parse heap sort, we need to declare all of its components. Therefore, we
    declare are currently declared sorts. *)
let sort_decls ctx =
  Sort.Set.elements ctx.sorts
  |> List.map (fun s -> Format.asprintf "(declare-sort %s 0)" (Sort.show s))
  |> String.concat "\n"

let parse_heap_sort ctx sort = match sort.term with
  (* Heap declaration *)
  | App (dom, [range]) ->
    let dom = SmtlibParser.parse_sort ctx dom in
    let range = SmtlibParser.parse_sort ctx range in
    let heap_sort = Sort.mk_array dom range in
    Context.declare_heap_sort ctx heap_sort

  (* Skip sort declarations *)
  | Builtin Ttype -> ctx

let parse_decls ctx decls =
  (* There should be no recursive group *)
  match List.hd decls.contents with
  | Abstract def ->
    begin match def.ty.term with
      | Binder (Arrow, [], sort) -> parse_heap_sort ctx sort
    end

let fake_input ctx heap_sort =
  let decls = sort_decls ctx in
  Format.asprintf "%s\n(declare-const heap %s)\n" decls heap_sort

let parse ctx heap_sort =
  let input = fake_input ctx heap_sort in
  (*Format.printf "%s" input;*)
  let stmts = SmtlibParser.parse_statements input in
  List.fold_left
    (fun ctx stmt -> match stmt.descr with
      | Decls decls -> parse_decls ctx decls
    ) ctx stmts
