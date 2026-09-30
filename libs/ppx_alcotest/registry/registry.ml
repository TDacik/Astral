(* Registry for inline tests.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

type test = {
  name : string;
  run : unit -> unit;
}

type suite = {
  name : string;
  tests : test list;
}

let current_suite = ref {name="default"; tests=[]}

let tests = ref []

let set_suite name =
  (if !current_suite.tests <> [] then
    tests := !current_suite :: !tests;
  );
  current_suite := {name; tests=[]}

let register_test ~name run =
  current_suite := {!current_suite with tests = {name; run} :: !current_suite.tests}

let gen_test (test : test) = Alcotest.test_case test.name `Quick test.run

let to_alcotest () =
  List.rev !tests
  |> List.map (fun suite -> (suite.name, List.rev @@ List.map gen_test suite.tests))

let register (name : string) : unit =
  tests := !current_suite :: !tests;
  Alcotest.run name (to_alcotest ())
