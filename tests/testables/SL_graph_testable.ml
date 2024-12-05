include SL_graph

let check_path _ ~actual ~expected =
  let msg = "TODO: compactly show SL-graph" in
  Alcotest.check' (module Interval : TESTABLE with type t = Interval.t) ~msg ~actual ~expected
