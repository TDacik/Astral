(* Parser of SMTlib models
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Dolmen_std
open Term
open Statement
open Dolmen_smtlib2.Script.Latest

module Extension = struct
  let statement _ = None
end

module Parser = Make(Loc)(Id)(Term)(Statement)(Extension)

module Logger = Logger.Make(struct let name = "ModelParser" let level = 1 end)

let parse_id id = Format.asprintf "%a" Id.print id
let parse_sort_name = function
  | "Int" -> Sort.int
  | "Bool" -> Sort.bool
  | bv when Str.string_match (Str.regexp {|(_ BitVec \([0-9]+\))|}) bv 0 ->
    Sort.mk_bitvector @@ int_of_string @@ Str.matched_group 1 bv
  | name -> Sort.mk_uninterpreted name

let rec parse_sort sort = match sort.term with
  | Binder (Arrow, [], sort) ->
    begin match sort.term with
      (* Sort of all sorts *)
      | Builtin Ttype ->
        raise Exit
      | Symbol id -> parse_sort_name @@ parse_id id
    end
  | Symbol id -> parse_sort_name @@ parse_id id
  | App (_, [set]) -> Sort.mk_set (parse_sort set)
  | App (_, [dom; range]) -> Sort.mk_array (parse_sort dom) (parse_sort range)

let rec parse_interp term sort = match term.term with
  | Symbol id -> begin match parse_id id with
    | numeral when Str.string_match (Str.regexp "[0-9]+") numeral 0 ->
      Constant.mk_int (int_of_string numeral)
    | bv when Str.string_match (Str.regexp {|#b\([0-9]\|[A-F]\)+|}) bv 0 ->
      Constant.mk_bitvector_of_string bv
    | other -> failwith ("Not implemented symbol: '" ^ other ^ "'")
  end
  | Colon (term, sort) -> parse_interp term (parse_sort sort)
  | App (fn, ts) -> begin match Format.asprintf "%a" Dolmen_std.Term.print fn with
    | "set.singleton" ->
      let elem = parse_interp (List.hd ts) sort in
      Constant.mk_set [elem]
    | "set.empty" -> Constant.mk_set []
    | "set.union" ->
      let set1 = Constant.get_elems @@ parse_interp (List.nth ts 0) sort in
      let set2 = Constant.get_elems @@ parse_interp (List.nth ts 1) sort in
      Constant.mk_set (set1 @ set2)
    | "store" ->
      let arr = parse_interp (List.nth ts 0) sort in
      let i = parse_interp (List.nth ts 1) sort in
      let v = parse_interp (List.nth ts 2) sort in
      Constant.array_add_binding arr i v
    | "const" ->
      let default = parse_interp (List.hd ts) sort in
      Constant.mk_array ~default [] (* TODO: is the sort correct? *)
    | other -> failwith (Format.asprintf "Unknown application: '%s'" other)
  end
  | _ -> failwith (Format.asprintf "Not implemented term: %a" Term.print term)


let parse_def model (def : Statement.def) =
    let name = parse_id def.id in
    let sort = parse_sort def.ret_ty in
    let interp = parse_interp def.body sort in
    SMT.Model.add (SMT.Variable.mk name sort) interp model

let parse_definitions model defs =
  if defs.recursive then failwith "Recursive definition in model"
  else
    assert (List.length defs.contents == 1);
    parse_def model (List.hd defs.contents)

let parse_string path =
  let stmts = Lazy.force @@ snd @@ Parser.parse_all (`Contents ("", path)) in
  List.fold_left (fun model stmt -> match stmt.descr with
    | Defs defs -> parse_definitions model defs
    | _ -> failwith "Unexpected statement in response"
  ) SMT.Model.empty stmts
