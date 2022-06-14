(* cvc5 adapter for Astral
 *
 * Building a cvc5 adapter is not so easy as for Z3 since there it does not have an OCaml API.
 * Instead, we translate Astral's representation of FOL into a string in SMTlib2 format that is
 * saved and passed to cvc5.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Adapter_sig

type t = string

let translate = function
  | Variable (x, _) -> x

  (* Boolean logic *)
  | And (e1, e2) -> Format.asprintf "(and %s %s)" (translate e1) (translate e2)
  | Or (e1, e2) -> Format.asprintf "(or %s %s)" (translate e1) (translate e2)
  | Not e -> Format.asprintf "(not e)" (translate e)

  (* Sets *)
  | Union (e1, e2) -> Format.asprintf "(set.union %s %s)" (translate e1) (translate e2)
  | Inter (e1, e2) -> Format.asprintf "(set.inter %s %s)" (translate e1) (translate e2)
  | Diff _ -> failwith "..."
  | Compl e -> Format.asprintf "(set.complement %s)" (translate e)

  (* Arrays *)
  | Select (a, i) -> Format.asprintf "(select %s %s)" (translate a) (translate i)
  | Store (a, i, x) ->
      Format.asprintf "(store %s %s %s)" (translate a) (translate i) (translate x)

  (* Linear integer arithmetics *)
  | Plus (e1, e2) -> Format.asprintf "(+ %s %s)" (translate e1) (translate e2)

let solve phi =
  let query_filename, query_channel = Filename.open_temp_file "cvc_query" ".smt2" in
  let answer_filename = Filename.temp_file "cvc_answer" ".txt" in
  Printf.fprintf query_channel "%s" (translate phi);
  close_out query_ channel;
  let retcode = Sys.command ("cvc5 " ^ query_filename ^ " > " ^ answer_filename) in

  (* Read answer *)
  let channel = open_in answer_filename in
  match input_line channel with
  | "sat" -> Sat
  | "unsat" -> Unsat
