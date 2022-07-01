(* Parser of SMTlib models
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Dolmen
open Dolmen_smtlib2.Latest

module Parser = Make
  (Std.Loc)
  (Std.Id)
  (Std.Term)
  (Std.Statement)

open Std.Id
open Std.Term
open Std.Statement

let rec parse_sort sort = match sort.term with
  | Builtin b -> begin match b with
    | Bool -> SMT.Bool
    | Int -> SMT.Integer
  end
  | App (t1, [t2]) -> SMT.Set (parse_sort t2)
  | App (t1, [t2; t3]) -> SMT.Array (parse_sort t2, parse_sort t3)
  | Symbol id -> SMT.Finite (Format.asprintf "%a" Std.Term.print sort, [])
  | _ -> failwith (Format.asprintf "%a" Std.Term.print sort)

let parse_symbol symbol sort =
  let name = Format.asprintf "%a" Dolmen_std.Id.print symbol in
  match symbol.ns with
  | Value v -> begin match v with
    | Integer -> SMT.LIA.mk_const @@ int_of_string name
    | _ -> failwith "Unsoppurted value"
  end
  | Term -> begin match name with
    | "set.empty" -> SMT.Set.mk_empty sort
    | name -> SMT.Enumeration.mk_const sort name
  end
  | _ -> SMT.Boolean.mk_var "symbol-other"

let rec parse_term term sort = match term.term with
  | Symbol id -> parse_symbol id sort
  | Colon (t, _) -> parse_term t sort

  (* Set singleton || Constant array *)
  | App (fn, [t]) ->
      begin match Format.asprintf "%a" Dolmen_std.Term.print fn with
      | "set.singleton" ->
        let elem = parse_term t sort in
        SMT.Enumeration ([elem], sort)
      | "const" ->
        let const = parse_term t sort in
        SMT.ConstArr const

      end

  (* Set insert *)
  | App (_, [t1; t2]) ->
      let set1 = SMT.Set.get_elems @@ parse_term t1 sort in
      let set2 = SMT.Set.get_elems @@ parse_term t2 sort in
      SMT.Enumeration (set1 @ set2, sort)
  (* Array store *)
  | App (_, [t1; t2; t3]) ->
      let arr = parse_term t1 sort in
      let i = parse_term t2 sort in
      let v = parse_term t3 sort in
      SMT.Store (arr, i, v)

  | _ -> SMT.Boolean.mk_var "TODO"


let parse_def (def : Std.Statement.def) =
  let name = Format.asprintf "%a" Dolmen_std.Id.print def.id in
  let name =
    if String.contains name ' '
    then "|" ^ name ^ "|"
    else name
  in
  let sort = parse_sort def.ret_ty in
  let interp = parse_term def.body sort in
  ((name, sort), interp)

let parse_stmt stmt = match stmt.descr with
  | Defs defs -> List.map parse_def defs.contents
  | _ -> failwith "Unexpected statement"

let parse str =
  let _, fn, _ = Parser.parse_input (`Contents ("model.smt2", str)) in
  let rec unpack generator acc =
    match generator () with
    | None -> acc
    | Some x -> unpack generator (x :: acc)
  in
  let stmts = unpack fn [] in
  let defs = List.concat @@ List.map parse_stmt stmts in
  List.fold_left
    (fun model (var, interp) -> SMT.Model.add var interp model)
    SMT.Model.empty
    defs
