(* Generator of random formulae
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open Batteries

(* === Base generators === *)

let gen_var var = SSL.Var (var, Sort.Loc)

let gen_eq (x, y) = SSL.Eq (x, y)
let gen_neq (x, y) = SSL.Neq (x, y)
let gen_pt (x, y) = SSL.PointsTo (x, y)
let gen_ls (x, y) = SSL.LS (x, y)

let gen_and f g = SSL.And (f, g)
let gen_or f g = SSL.Or (f, g)
let gen_gneg f g = SSL.GuardedNeg (f, g)
let gen_star f g = SSL.Star (f, g)
let gen_septraction f g = SSL.Septraction (f, g)

module type PARAMS = sig
  val n_vars : int
  val depth : int
  val lists : bool
  val unfold : bool
end

module Make (Params : PARAMS) = struct

  let variables =
    List.range 1 `To Params.n_vars
    |> List.map (fun i -> Format.asprintf "x%d" i)

  let gen_var_name = QCheck.Gen.oneofl variables

  let gen_var = QCheck.Gen.map gen_var gen_var_name

  let gen_var_nil =
    QCheck.Gen.oneof [QCheck.Gen.map Variable.mk gen_var_name; fun _ -> Variable.nil]

  let gen_var_pair = QCheck.Gen.pair gen_var gen_var

  let gen_atom = QCheck.Gen.oneof ([
    (*QCheck.Gen.map gen_eq gen_var_pair;
    QCheck.Gen.map gen_neq gen_var_pair;*)
    QCheck.Gen.map gen_pt gen_var_pair;
  ] @ if Params.lists then [QCheck.Gen.map gen_ls gen_var_pair] else [])

  let bound_gen = QCheck.Gen.int_range Params.depth Params.depth

  let gen_formula = QCheck.Gen.(sized_size bound_gen @@ fix
    (fun self n -> match n with
      | 0 -> gen_atom
      | n ->
          if true then
          (QCheck.Gen.oneof [
              QCheck.Gen.map2 gen_and  (self (n/2)) (self (n/2));
              QCheck.Gen.map2 gen_star (self (n/2)) (self (n/2));
              QCheck.Gen.map2 gen_gneg (self (n/2)) (self (n/2));
              QCheck.Gen.map2 gen_or (self (n/2)) (self (n/2));
              (*QCheck.Gen.map2 gen_septraction (self (n/2)) (self (n/2))*)
            ])
          else (QCheck.Gen.oneof [
            QCheck.Gen.map2 gen_and  (self (n/2)) (self (n/2));
            QCheck.Gen.map2 gen_or   (self (n/2)) (self (n/2));
            QCheck.Gen.map2 gen_star (self (n/2)) (self (n/2));
            QCheck.Gen.map2 gen_gneg (self (n/2)) (self (n/2));
            (*QCheck.Gen.map2 gen_septraction (self (n/2)) (self (n/2))*)
          ])
    ))

  let arbitrary_formula = QCheck.make @@ QCheck.Gen.map2 gen_gneg gen_formula gen_formula
  (*let arbitrary_formula = QCheck.make gen_formula*)

  (* === Benchmark generation === *)

  let counter = ref 0

  let next () =
    counter := !counter + 1;
    !counter

  let check phi = true
    (*let res = Solver.solve phi (SSL.get_vars phi) ~verify_model:true in
    match res.status with
    | `SAT -> true (*Option.get res.model_verified*)
    | `UNSAT -> true
    *)
  let dump_assert store prefix phi =
    Format.printf "Testing: %a\n" SSL.pp phi;
    let res = check phi in
    if store then begin
      let path = Format.asprintf "%s%d.smt2" prefix (next ()) in
      let phi =
        if Params.unfold then Predicate_unfolding.unfold phi 3 (* TODO: bound *)
        else phi
      in
      Smtlib_convertor.dump path phi "unknown"
    end;
    res

  let generate n store prefix =
    QCheck.Test.make ~count:n arbitrary_formula (dump_assert store prefix)

  let generate n store prefix =
    QCheck.Test.check_exn (generate n store prefix)

end
