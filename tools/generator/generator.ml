(* Generator of random SL formulae.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let read_param_int message =
  Printf.printf "%s: " message;
  read_int ()

let rec read_param_bool message =
  Printf.printf "%s(y/n): " message;
  match read_line () with
  | "y" -> true
  | "n" -> false
  | _ -> read_param_bool message

let read_param_string message =
  Printf.printf "%s: " message;
  read_line ()

(** Benchmark generation *)

let counter = ref 0

let next () =
  counter := !counter + 1;
  !counter

let is_ok phi =
  let g = SL_graph.compute phi in
  let lhs, rhs = SSL.as_entailment phi in
  let lhs_vars = SSL.Set.of_list @@ SSL.free_vars lhs in
  let rhs_vars = SSL.Set.of_list @@ SSL.free_vars rhs in
  let lists = SSL.select_subformulae (function SSL.LS _ -> true | _ -> false) phi in
  let c1 = SSL.Set.subset rhs_vars lhs_vars in
  let c2 = not @@ SL_graph.has_contradiction g in
  let c3 = List.for_all (function SSL.LS (x, y) -> not @@ SSL.equal x y | _ -> assert false) lists in
  let res = c1 && c2 in
  if res
  then Format.printf "Good %b %b %b\n" c1 c2 c3
  else Format.printf "Bad %b %b %b\n" c1 c2 c3;
  res


let dump_assert prefix phi =
  QCheck2.assume (is_ok phi);
  let path = Format.asprintf "%s%d.smt2" prefix (next ()) in
  if !counter <= 100 then SSLDumper.dump path phi "unknown" else ();
  true
  (*
  if is_ok phi then
    let path = Format.asprintf "%s%d.smt2" prefix (next ()) in
    SSLDumper.dump path phi "unknown";
    Format.printf "Good %d\n" !counter;
    true
  else true
  *)

let generate arbitrary n prefix =
    let qcheck = QCheck2.Test.make ~count:n arbitrary (dump_assert prefix) in
    let _ = QCheck_runner.run_tests ~verbose:false [qcheck] in
    ()
  (*while !counter <= 1000 do
    let qcheck = QCheck.Test.make  arbitrary (dump_assert prefix) in
    let _ = QCheck_runner.run_tests ~verbose:false [qcheck] in
    ()
  done*)

(** Interactive main *)

let () =
  Printf.printf "Running in interactive mode\n";
  let n_benchmarks    = read_param_int "Number of generated formulae" in
  let n_vars          = read_param_int "Number of variables" in
  let n_selectors     = read_param_int "Number of selectors" in
  let depth_min       = read_param_int "Minimal depth" in
  let depth_max       = read_param_int "Maximal depth" in
  let lists           = read_param_bool "Include ls predicates" in
  let star_arity_min  = read_param_int "Star arity min" in
  let star_arity_max  = read_param_int "Star arity max" in
  let formula_prefix  = read_param_string "Prefix" in

  let module Params = struct
    let n_vars = n_vars
    let n_selectors = n_selectors, n_selectors
    let depth = depth_min, depth_max
    let unfold = false
    let lists = lists
    let star_arity = star_arity_min, star_arity_max
  end
  in

  let module Arbitrary = ArbitrarySSL.Make(Params) in
  generate Arbitrary.entailment n_benchmarks formula_prefix
