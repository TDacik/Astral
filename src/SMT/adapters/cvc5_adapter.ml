(* cvc5 adapter for Astral
 *
 * Building a cvc5 adapter is not so easy as for Z3 since there it does not have an OCaml API.
 * Instead, we translate Astral's representation of FOL into a string in SMTlib2 format that is
 * saved and passed to cvc5.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

type t = string

let defs = ref []

let declarations = ref ""

let add_declaration str =
  if List.mem str !defs
  then ()
  else
    declarations := !declarations ^ "\n" ^ str;
    defs := str :: !defs

let rec translate term = match term with
  | SMT.Variable (x, sort) ->
      let def = Format.asprintf "(declare-const %s %s)" x (translate_sort sort) in
      add_declaration def;
      x
  | SMT.True -> "true"
  | SMT.False -> "false"
  | SMT.Equal (e1, e2) -> Format.asprintf "(= %s %s)" (translate e1) (translate e2)
  | SMT.Distinct es -> Format.asprintf "(distinct %s)" (translate_expr_list es)
  | SMT.And es -> Format.asprintf "(and %s)" (translate_expr_list es)
  | SMT.Or es -> Format.asprintf "(or %s)" (translate_expr_list es)
  | SMT.Not e -> Format.asprintf "(not %s)" (translate e)
  | SMT.Implies (e1, e2) -> Format.asprintf "(=> %s %s)" (translate e1) (translate e2)
  | SMT.Iff (e1, e2) -> Format.asprintf "(= %s %s)" (translate e1) (translate e2)

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

  | SMT.ConstArr const -> failwith "not implemented"
  | SMT.Select (a, i) -> Format.asprintf "(select %s %s)" (translate a) (translate i)
  | SMT.Store (a, i, x) ->
    Format.asprintf "(store %s %s %s)" (translate a) (translate i) (translate x)

  | SMT.Forall (x, phi) ->
    Format.asprintf "(forall ((%s %s)) %s)"
      (translate x)
      (translate_sort @@ SMT.Term.get_sort x)
      (translate phi)

  | SMT.Exists (x, phi) ->
    Format.asprintf "(exists ((%s %s)) %s)"
      (translate x)
      (translate_sort @@ SMT.Term.get_sort x)
      (translate phi)

  | SMT.Plus (e1, e2) -> Format.asprintf "(+ %s %s)" (translate e1) (translate e2)

and translate_sort = function
  | SMT.Bool -> "Bool"
  | SMT.Integer -> "Int"
  | SMT.Set (elem_sort) -> "(Set " ^ translate_sort elem_sort ^ ")"
  | SMT.Array (d, r) -> "(Array " ^ (translate_sort d) ^ " " ^ (translate_sort r) ^ ")"
  | SMT.Finite (name, consts) ->
      (* Datatype with constant constructors only *)
      let constructors = String.concat " " @@ List.map (fun c -> "(loc" ^ SMT.Term.to_string c ^ ")") consts in
      let decl = "(declare-datatypes ((Loc 0)) ((" ^ constructors ^ ")))" in
      add_declaration decl;
      name

and translate_expr_list exprs =
  List.map translate exprs
  |> String.concat " "

let solve phi =
  let smt_query = Format.asprintf "(set-logic ALL)\n%s\n(assert %s)\n(check-sat)"
    !declarations
    (translate phi)
  in
  let query_filename, query_channel = Filename.open_temp_file "cvc_query" ".smt2" in
  let answer_filename = Filename.temp_file "cvc_answer" ".txt" in
  Printf.fprintf query_channel "%s" smt_query;
  close_out query_channel;
  let retcode = Sys.command ("cvc5 " ^ query_filename ^ " > " ^ answer_filename) in

  (* Read answer *)
  let channel = open_in answer_filename in
  match input_line channel with
  | "sat" -> SMT.SMT_Sat
  | "unsat" -> SMT.SMT_Unsat
  | other -> failwith other
