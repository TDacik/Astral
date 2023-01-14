(* cvc5 adapter for Astral
 *
 * Building a cvc5 adapter is not so easy as for Z3 since there it does not have an OCaml API.
 * Instead, we translate Astral's representation of FOL into a string in SMTlib2 format that is
 * saved and passed to cvc5.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Backend_sig
open Solver_utils

module Batlist = Batteries.List

(* === Declarations === *)

type formula = string

type model = string

let name = "cvc5"

(* === Initialization === *)

let defs = ref []

let declarations = ref ""

let is_available () =
  match Sys.command "cvc5 --version >/dev/null 2>/dev/null" with
  | 0 -> true
  | _ -> false

let init () =
  if not @@ is_available () then raise Not_available;
  defs := [];
  declarations := ""

let add_declaration str =
  if List.mem str !defs
  then ()
  else
    declarations := !declarations ^ "\n" ^ str;
    defs := str :: !defs

(* === Translation === *)

let rec translate_decl (SMT.Variable (name, sort)) =
  Format.asprintf "(declare-const %s %s)" name (translate_sort sort)

and translate term =
  match term with
  | SMT.Variable (name, _) -> name
  | SMT.Constant (name, _) -> "|" ^ name ^ "|"

  | SMT.True -> "true"
  | SMT.False -> "false"
  | SMT.Equal (e1, e2) -> Format.asprintf "(= %s %s)" (translate e1) (translate e2)
  | SMT.Distinct es -> Format.asprintf "(distinct %s)" (translate_expr_list es)

  | SMT.And es -> Format.asprintf "(and %s)" (translate_expr_list es)
  | SMT.Or es -> Format.asprintf "(or %s)" (translate_expr_list es)
  | SMT.Not e -> Format.asprintf "(not %s)" (translate e)
  | SMT.Implies (e1, e2) -> Format.asprintf "(=> %s %s)" (translate e1) (translate e2)
  | SMT.Iff (e1, e2) -> Format.asprintf "(= %s %s)" (translate e1) (translate e2)

  | SMT.LesserEq (e1, e2) -> begin match SMT.Term.get_sort e1 with
    | Sort.Bitvector _ -> Format.asprintf "(bvule %s %s)" (translate e1) (translate e2)
    (* TODO: other sorts *)
  end

  | SMT.Membership (e1, e2) ->
    Format.asprintf "(set.member %s %s)" (translate e1) (translate e2)
  | SMT.Subset (e1, e2) -> Format.asprintf "(set.subset %s %s)" (translate e1) (translate e2)
  | SMT.Union (es, _) -> Format.asprintf "(set.union %s)" (translate_expr_list es)
  | SMT.Inter (es, _) -> Format.asprintf "(set.inter %s)" (translate_expr_list es)
  | SMT.Diff (e1, e2) -> Format.asprintf "(set.minus %s %s)" (translate e1) (translate e2)
  | SMT.Compl e -> Format.asprintf "(set.complement %s)" (translate e)
  | SMT.Disjoint (e1, e2) ->
    Format.asprintf "(= (set.inter %s %s) (as set.empty %s))"
      (translate e1)
      (translate e2)
      (translate_sort @@ SMT.Set.get_sort e1)

  | SMT.Enumeration (es, sort) ->
    begin match es with
    | [] -> Format.asprintf "(as set.empty %s)" (translate_sort sort)
    | _  -> Format.asprintf "(set.insert %s (as set.empty %s))"
      (translate_expr_list es)
      (translate_sort sort)
    end

  | SMT.ConstArr (const, _) -> failwith "not implemented"
  | SMT.Select (a, i) -> Format.asprintf "(select %s %s)" (translate a) (translate i)
  | SMT.Store (a, i, x) ->
    Format.asprintf "(store %s %s %s)" (translate a) (translate i) (translate x)

  (* Bitvectors *)
  | SMT.BitConst (number, width) ->
      Format.asprintf "(_ bv%d %d)" number width
  | SMT.BitCheck (bv, index) ->
      let width = SMT.Bitvector.get_width bv in
      Format.asprintf "(distinct (bvand (bvshl (_ bv1 %d) %s) %s) (_ bv0 %d))"
        width
        (translate index)
        (translate bv)
        width

  (* TODO *)
  | SMT.BitAnd ([bv1; bv2], sort) ->
      Format.asprintf "(bvand %s %s)" (translate bv1) (translate bv2)

  | SMT.BitOr (bvs, Sort.Bitvector n) ->
      let zeros = Format.asprintf "(_ bv0 %d)" n in
      List.fold_left (fun acc bv -> Format.asprintf "(bvor %s %s)" acc (translate bv)) zeros bvs

  | SMT.BitXor ([bv1; bv2], sort) ->
      Format.asprintf "(bvxor %s %s)" (translate bv1) (translate bv2)
  | SMT.BitImplies (bv1, bv2) ->
      Format.asprintf "(bvor (bvnot %s) %s)" (translate bv1) (translate bv2)
  | SMT.BitCompl bv ->
      Format.asprintf "(bvnot %s)" (translate bv)
  | SMT.BitShiftLeft (bv, rotate) ->
      Format.asprintf "(bvshl %s %s)" (translate bv) (translate rotate)
  | SMT.BitShiftRight (bv, rotate) ->
      Format.asprintf "(bvlshr %s %s)" (translate bv) (translate rotate)

  | SMT.Sequence (seq, sort) ->
    let empty = Format.asprintf "(as seq.empty (%s))" (translate_sort sort) in
    List.fold_left
      (fun acc x -> Format.asprintf "(seq.++ (seq.unit %s) %s)" acc (translate x)) empty seq

  | SMT.SeqIndex (seq, index) ->
      Format.asprintf "(seq.nth %s %s)" (translate seq) (translate index)
  | SMT.SeqContains (elem, seq) ->
      Format.asprintf "(seq.contains (seq.unit %s) %s)" (translate elem) (translate seq)
  | SMT.SeqReverse seq ->
      Format.asprintf "(seq.reverse %s)" (translate seq)

  (* TODO: instantiation should be done somewhere else *)
  (* TODO: !!!!!!! *)
  | SMT.Forall (xs, phi) ->
    let x_sort = SMT.Term.get_sort (List.hd xs) in
    begin match x_sort with
    | Finite (_, cs) ->
      let es = List.map (fun c -> SMT.Term.substitute phi (List.hd xs) (SMT.Constant (c, x_sort))) cs in
      Format.asprintf "(and %s)" (translate_expr_list es)
    | _ ->
      Format.asprintf "(forall (%s) %s)"
      (List.map translate_binder xs |> String.concat " ")
      (translate phi)
    end

  | SMT.Exists (xs, phi) ->
    let x_sort = SMT.Term.get_sort (List.hd xs) in
    begin match x_sort with
    | Finite (_, cs) ->
      let es = List.map (fun c -> SMT.Term.substitute phi (List.hd xs) (SMT.Constant (c, x_sort))) cs in
      Format.asprintf "(or %s)" (translate_expr_list es)
    | _ ->
      Format.asprintf "(exists (%s) %s)"
        (List.map translate_binder xs |> String.concat " ")
        (translate phi)
    end

  | SMT.IntConst i -> Format.asprintf "%d" i
  | SMT.Plus (e1, e2) -> Format.asprintf "(+ %s %s)" (translate e1) (translate e2)
  | SMT.Minus (e1, e2) -> Format.asprintf "(- %s %s)" (translate e1) (translate e2)
  | SMT.Mult (e1, e2) -> Format.asprintf "(* %s %s)" (translate e1) (translate e2)

  | SMT.Forall2 _ | SMT.Exists2 _ ->
      failwith "Internal error: second order quantification should be removed before \
                translating to backend solver"

and translate_binder (Variable (name, sort)) =
  Format.asprintf "(%s %s)" name (translate_sort sort)

and translate_sort = function
  | Sort.Bool -> "Bool"
  | Sort.Int -> "Int"
  | Sort.Bitvector n -> Format.asprintf "(_ BitVec %d)" n
  | Sort.Set (dom_sort) -> "(Set " ^ translate_sort dom_sort ^ ")"
  | Sort.Sequence (dom_sort) -> "(Sequence " ^ translate_sort dom_sort ^ ")"
  | Sort.Array (d, r) -> "(Array " ^ (translate_sort d) ^ " " ^ (translate_sort r) ^ ")"
  | Sort.Finite (name, consts) ->
      (* Datatype with constant constructors only *)
      let constructors = String.concat " " @@ List.map (fun c -> "(|" ^ c ^ "|)") consts in
      let decl = "(declare-datatypes ((Loc 0)) ((" ^ constructors ^ ")))" in
      add_declaration decl;
      name

and translate_expr_list exprs =
  List.map translate exprs
  |> String.concat " "

let simplify phi = phi

(* === Solver === *)

let cvc5_options user_options = match user_options with
  | [] -> [| "--produce-models"; "--sygus-inst" |]
  | options -> Array.of_list options

let solve phi produce_models options =
  let vars = SMT.free_vars phi |> List.map translate_decl |> String.concat "\n" in
  let smt_query = Format.asprintf
  "(set-logic ALL)\n(set-option :produce-models true)\n%s\n%s\n(assert %s)\n(check-sat)\n(get-info :reason-unknown)\n(get-model)"
    !declarations
    vars
    (translate phi)
  in
  let query_filename, query_channel = Filename.open_temp_file "cvc_query" ".smt2" in
  let answer_filename, answer_channel = Filename.open_temp_file "cvc_answer" ".txt" in
  Printf.fprintf query_channel "%s" smt_query;
  close_out query_channel;

  let input = Unix.descr_of_in_channel @@ open_in query_filename in
  let output = Unix.descr_of_out_channel answer_channel in
  let options = cvc5_options options in
  let pid =
    Unix.create_process
      "cvc5"
      options
      input
      output
      Unix.stderr
  in

  (* Register cleaning action *)
  let clean = Sys.Signal_handle
    (fun _ ->
      Unix.kill pid Sys.sigkill;
      Printf.printf "interrupted";
      exit 0
    )
  in
  Sys.set_signal Sys.sigint clean;

  (* Wait for the result *)
  let _ = Unix.wait () in
  close_out answer_channel;

  (* Read answer *)
  let channel = open_in answer_filename in
  let status_line = input_line channel in
  let reason_unknown = input_line channel in

  (* TODO: update to In_channel.input_all ...; in ocaml 4.14 *)
  let rec input_all channel acc =
    try
      input_all channel (acc ^ "\n" ^ input_line channel)
    with End_of_file -> acc
  in

  let model =
    input_all channel ""
    |> String.split_on_char '\n'
    |> Batlist.drop 2
    |> List.rev
    |> Batlist.drop 1
    |> List.rev
    |> String.concat "\n"
  in

  match status_line with
  | "sat" ->
    if produce_models
    then SMT_Sat (Some (ModelParser.parse model, model))
    else SMT_Sat None
  | "unsat" -> SMT_Unsat [] (* TODO: unsat core *)
  | "unknown" -> SMT_Unknown reason_unknown
  | error -> failwith ("[ERROR cvc5] " ^ error)

(* === Debugging === *)

(** Formula is a smtlib string. *)
let show_formula phi = phi

(** Model is a smtlib string. *)
let show_model model = model

let to_smt_benchmark _ = "; Not available for cvc5 backend"
