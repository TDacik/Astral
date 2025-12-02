(* Tests for path bound computation.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open MemoryModel0

module V = SL_testable
module G = SL_graph_testable


(** Self-loop on x implies that each path from x should be empty. *)
let path_starts_at_self_loop () =
  let sl_graph =
    G.empty
    |> fun g -> G.add_edge_e g (V.x, Pointer Field.next, V.x)
    |> fun g -> G.add_edge_e g (V.y, Pointer Field.next, V.z)
  in
  PathBound.cache_reset ();
  let lower_bound = PathBound.path_lower_bound sl_graph Field.next V.x V.y in
  let upper_bound = PathBound.path_upper_bound sl_graph Field.next V.x V.y 1000 in
  G.check_path sl_graph ~actual:(lower_bound, upper_bound) ~expected:(0, 1) (* TODO: should be (0, 0)? *)

(** SL-graph with : u -> v -> w ~~> x -> y *)
let no_direct_path () =
  let sl_graph =
    G.all_distinct [V.u; V.v; V.w; V.x; V.y; V.z]
    |> fun g -> G.add_edge_e g (V.u, Pointer Field.next, V.v)
    |> fun g -> G.add_edge_e g (V.v, Pointer Field.next, V.w)
    |> fun g -> G.add_edge_e g (V.x, Pointer Field.next, V.y)
  in
  PathBound.cache_reset ();
  let lower_bound = PathBound.path_lower_bound sl_graph Field.next V.u V.z in
  let upper_bound = PathBound.path_upper_bound sl_graph Field.next V.u V.z 10 in
  G.check_path sl_graph ~actual:(lower_bound, upper_bound) ~expected:(2, 10)

let () =
  run "PathBound" [
    "path", [
      test_case "No path (self-loop)" `Quick path_starts_at_self_loop;
      test_case "No direct path"      `Quick no_direct_path;
    ]]
