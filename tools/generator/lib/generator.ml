(* Generations of random formulae.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023

open SSLDumper

let counter = ref 0

let reset () =
  counter := 0

let next () =
  counter := !counter + 1;
  !counter

let dump_assert prefix phi =
  let path = Format.asprintf "%s%d.smt2" prefix (next ()) in
  SSLDumper.dump path phi "unknown";
  true

let generate arbitrary n prefix =
  let qcheck = QCheck.Test.make ~count:n arbitrary (dump_assert prefix) in
  let _ = QCheck_runner.run_tests [qcheck] in
  ()

*)
