(* Astral: solver for separation logics
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

let () =
  Astral.Profiler.add "Start";
  let input_file = Astral.Options.parse () in
  Astral.Debug.init (); (* Debug initialisation needs to be called after options' parsing *)
  Printexc.record_backtrace (Astral.Options.debug ());

  (* TODO: should be done elsewhere *)
  if Astral.Options.use_builtins () then begin
    Astral.LS.register ();
    Astral.DLS.register ();
    Astral.NLS.register ();
  end;

  let input = Parser.parse input_file in
  let result = Astral.Engine.solve input in
  Astral.Profiler.add "Solver";

  Reporter.report result;
  Checker.check result
