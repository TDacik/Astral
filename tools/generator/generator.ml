(* Generator of random SL formulae.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let read_param_int message =
  Printf.printf "%s: " message;
  read_int ()

let read_param_bool message =
  Printf.printf "%s(y/n): " message;
  match read_line () with
  | "y" -> true
  | "n" -> false

(** Benchmark generation *)

let counter = ref 0

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

(** Interactive main *)

let () =
  Printf.printf "Running in interactive mode\n";
  let n_benchmarks    = read_param_int "Number of generated formulae" in
  let n_vars          = read_param_int "Number of variables" in
  let n_selectors     = read_param_int "Number of selectors" in
  let depth_min       = read_param_int "Minimal depth" in
  let depth_max       = read_param_int "Maximal depth" in
  let unfold          = read_param_bool "Unfold predicates" in
  let star_arity_min  = read_param_int "Star arity min" in
  let star_arity_max  = read_param_int "Star arity max" in

  let module Params = struct
    let n_vars = n_vars
    let n_selectors = n_selectors, n_selectors
    let depth = depth_min, depth_max
    let unfold = unfold
    let lists = true
    let star_arity = star_arity_min, star_arity_max
  end
  in

  let module Arbitrary = ArbitrarySSL.Make(Params) in
  generate Arbitrary.qf_symbolic_heap_entl n_benchmarks "qf_shls_entailments"
