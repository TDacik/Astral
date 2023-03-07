(* Functor for building SMT-LIB compliant backends
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Backend_sig
open Generic_smtlib

module Make (Backend : SMTLIB_BACKEND) = struct

  include Backend

  (** Formula is represented as string in SMT-LIB format. *)
  type formula = string

  (** Model is represented as string in SMT-LIB format. *)
  type model = string

  (** Check whether solver is available. *)
  let is_available () =
    match Sys.command (binary ^ " --version >/dev/null 2>/dev/null") with
    | 0 -> true
    | _ -> false

  let init () = ()

  (** ==== Solver ==== *)

  (** No simplification. *)
  let simplify phi = phi

  let rec translate phi =
    try translate_std translate translate_sort phi
    with NonStandardTerm -> Backend.translate_non_std translate translate_sort phi

  and translate_sort sort =
    try translate_std_sort translate_sort sort
    with NonStandardTerm -> Backend.translate_non_std_sort translate_sort sort

  let translate_var_decl (SMT.Variable (name, sort)) =
    Format.asprintf "(declare-const %s %s)" name (translate_sort sort)

  let translate_sort_decl = function
    | Sort.Bool | Sort.Int | Sort.Array _ | Sort.Bitvector _ -> ""
    | sort -> Backend.declare_non_std_sort sort

  let translate_decls phi =
    let vars =
      SMT.free_vars phi
      |> List.map translate_var_decl
      |> String.concat "\n"
    in
    let sorts =
      SMT.get_all_sorts phi
      |> List.map translate_sort_decl
      |> String.concat "\n"
    in
    sorts ^ vars

  let get_logic phi = "ALL"
    (*
    if Options.encoding = "sets" then "ALL"
    else if Options.encoding = "bitvectors" then
      if SMT.is_quantifier_free phi then "QF_ABV"
      else "ABV"
    *)

  let generate_query phi produce_models options =
    let set_logic = Format.asprintf "(set-logic %s)" (get_logic phi) in
    let options = if produce_models then "(set-option :produce-models true)" else "" in
    let header = translate_decls phi in
    let assertion = Format.asprintf "(assert %s)" (translate phi) in
    let check_sat = "(check-sat)" in
    let get_reason_unknown =
      if Backend.supports_get_info then "(get-info :reason-unknown)" else ""
    in
    let get_model = if produce_models then "(get-model)" else "" in
    let exit_cmd = "(exit)" in
    Format.asprintf "%s\n%s\n\n%s\n\n%s\n%s\n%s\n%s\n%s\n"
      set_logic options
      header
      assertion
      check_sat get_reason_unknown get_model exit_cmd

  let solve context phi produce_models options =
    let smt_query = generate_query phi produce_models options in
    let query_name = Backend.name ^ "_query" in
    let answer_name = Backend.name ^ "_answer" in
    let query_filename, query_channel = Filename.open_temp_file query_name ".smt2" in
    let answer_filename, answer_channel = Filename.open_temp_file answer_name ".smt2" in
    Printf.fprintf query_channel "%s" smt_query;
    close_out query_channel;

    let input = Unix.descr_of_in_channel @@ open_in query_filename in
    let output = Unix.descr_of_out_channel answer_channel in
    let pid =
      Unix.create_process
        Backend.binary
        [| "--produce-models" |]
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
    Unix.close input;
    close_out answer_channel;

    (* Read answer *)
    let channel = open_in answer_filename in
    let status_line = input_line channel in
    let reason_unknown =
      if Backend.supports_get_info then input_line channel
      else "Not available"
    in

    (* TODO: this probably only works for cvc5 *)
    let model =
        In_channel.input_all channel
        |> String.split_on_char '\n'
        |> BatList.drop 2
        |> List.rev
        |> BatList.drop 1
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
        | error -> failwith ("[ERROR " ^ Backend.name ^ "] " ^ error)


  (* === Model manipulation === *)

  let eval = SMT.Model.eval


  (* === Debugging === *)

  let show_formula phi = phi

  let show_model model = model

  let to_smtlib phi produce_models options = generate_query phi produce_models options

end
