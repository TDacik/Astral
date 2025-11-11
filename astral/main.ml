(* Astral: solver for separation logics
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

open Utils

let run () =
  Astral.Profiler.add "Start";
  let input_file = Astral.Config.parse_cmdline () in

  Astral.SolverState.init (); (* Debug initialisation needs to be called after options' parsing *)
  Printexc.record_backtrace (Astral.Config.Debug.get ());
  Reporter.register_at_exit ();
  (* In case we are working with imprecise semantics of SL, we need to turn off
     simplification before parsing to do not apply simplification rules such as
     nil = nil ~> emp. *)
  (if Astral.Config.ImprecisePureAtoms.get () then Astral.BaseLogic.use_simplification false);

  let input = Parser.parse input_file in
  let result = Astral.Engine.solve input in
  Astral.Profiler.add "Solver";

  Reporter.report result;
  Checker.check result

let () =
  try run ()
  with
    | Astral.Exceptions.InternalError (trace, reason, details) ->
      Astral.Exceptions.pretty_internal_error reason ~trace ~details
    | Astral.Config.CmdOptionError msg ->
      user_error "%s\n" msg
    | Astral.BaseLogic.TypeError error ->
      Format.eprintf "Unhandled type error: %s\n" (Astral.BaseLogic.show_type_error error);
      Printexc.print_backtrace stderr
