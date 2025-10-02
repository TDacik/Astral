(* Astral: solver for separation logics
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

let run () =
  Astral.Profiler.add "Start";
  let input_file = Astral.Options.parse ~version:(BuildInfo.version ()) in
  Astral.Debug.init (); (* Debug initialisation needs to be called after options' parsing *)
  Astral.Logger_state.init ();
  Printexc.record_backtrace (Astral.Options.debug ());
  Reporter.register_at_exit ();

  (* In case we are working with imprecise semantics of SL, we need to turn off
     simplification before parsing to do not apply simplification rules such as
     nil = nil ~> emp. *)
  (if Astral.Options.semantics () != `Precise then Astral.BaseLogic.use_simplification false);

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
    | Astral.Exceptions.CmdOptionError _ -> ()

    | Astral.BaseLogic.TypeError error ->
      Format.eprintf "Unhandled type error: %s\n" (Astral.BaseLogic.show_type_error error);
      Printexc.print_backtrace stderr
