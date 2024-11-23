(* Model verification.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Astral
open Context
open ThreeValuedLogic

(** Check status against specification in the input. *)
let check_status result =
  let actual = Option.get result.status in
  let expected = result.raw_input.expected_status in
  if status_is_unknown actual || status_is_unknown expected then Unknown
  else if actual = expected then True
  else False

(** Check model using model checker.*)
let check_model result =
  Profiler.add "Model checker";
  if Options.verify_model () && Option.is_some result.model
  then match ModelChecker.check (Option.get result.model) result.phi with
    | Ok true -> Format.printf "Model verified\n"; True
    | Ok false -> Logger.error "Model is not correct\n"; False
    | Error (Unsupported msg) -> Logger.warning "%s\n" msg; Unknown
    | Error (Failure (msg, backtrace)) -> Logger.error (*~backtrace:backtrace*) "%s\n" msg; Unknown
  else Unknown

let check_result result status model = match status, model with
  | (True | Unknown), (True | Unknown) -> ()
  | False, False ->
    Utils.internal_error ~backtrace:false ~report:false ~exit_code:1
      ("Expected status is " ^ Context.show_expected_status result)
  | False, True ->
    Utils.internal_error ~backtrace:false ~report:false ~exit_code:3
      ("Expected status is " ^ Context.show_expected_status result ^ ", but model is correct!")
  | True, False ->
    Utils.internal_error ~backtrace:false ~report:false ~exit_code:4
      ("Generated model is not correct.")

let check result =
  let status = check_status result in
  let model = check_model result in
  check_result result status model
