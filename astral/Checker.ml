(* Model verification.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Astral
open Context
open ThreeValuedLogic

let print_error fmt =
  Format.kasprintf (fun msg ->
    if Unix.isatty Unix.stderr
    then Format.eprintf "%s%s%s" Colors.red msg Colors.white
    else Format.eprintf "%s\n" msg
  ) fmt

let user_error fmt ~exit_code msg =
  Format.kasprintf (fun msg ->
    if Unix.isatty Unix.stderr
    then Format.eprintf "%s%s%s" Colors.red msg Colors.white
    else Format.eprintf "%s\n" msg
  ) fmt

let internal_error ?(backtrace=true) ~exit_code msg =
  let stack = Printexc.get_callstack 1000000 in
  Format.eprintf "%s[Internal error]%s %s\n"
    Colors.red Colors.white msg;
  if backtrace then begin
    Format.eprintf "\nBacktrace:\n%s"
      (Printexc.raw_backtrace_to_string stack)
  end;
  exit exit_code

(** Check status against specification in the input. *)
let check_status result =
  let actual = Option.get result.status in
  let expected = result.raw_input.expected_status in
  if status_is_unknown actual || (expected = `Unknown) then Unknown
  else match actual, expected with
    | `Sat, `Sat | `Unsat, `Unsat -> True
    | _ -> False

(** Check model using model checker.*)
let check_model result =
  Profiler.add "Model checker";
  if Options.verify_model () && Option.is_some result.model
  then match ModelChecker.check (Option.get result.model) result.phi with
    | Ok true -> Format.printf "Model verified\n"; True
    | Ok false -> print_error "Model is not correct\n"; False
    | Error (Unsupported msg) -> Utils.warning "%s\n" msg; Unknown
    | Error (Failure (exc, backtrace)) ->
      let msg = Printexc.to_string exc in
      print_error "%s" msg;
      Unknown
  else Unknown

let check_result result status model = match status, model with
  | (True | Unknown), (True | Unknown) -> ()
  | (False), (False | Unknown) ->
    internal_error ~backtrace:false ~exit_code:1
      ("Expected status is " ^ Context.show_expected_status result)
  | False, True ->
    internal_error ~backtrace:false ~exit_code:3
      ("Expected status is " ^ Context.show_expected_status result ^ ", but model is correct!")
  | True, False ->
    internal_error ~backtrace:false ~exit_code:4
      ("Generated model is not correct.")
  | _ -> ()

let check result =
  let status = check_status result in
  let model = check_model result in
  check_result result status model
