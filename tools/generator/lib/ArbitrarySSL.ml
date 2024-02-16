(* Generator of random formulae
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open Batteries

module QCheck = QCheck2
open QCheck

(* === Base generators === *)

(* TODO: sorts *)
let gen_var var = SSL.mk_var var (Sort.loc_ls)
let gen_eq x y = SSL.mk_eq x y
let gen_neq x y = SSL.mk_distinct x y
let gen_pto x y = SSL.mk_pto x y
let gen_ls x y = SSL.mk_ls x y
let gen_dls x y f l = SSL.mk_dls x y f l

let gen_and f g = SSL.mk_and [f; g]
let gen_or f g = SSL.mk_or [f; g]
let gen_gneg f g = SSL.mk_gneg f g
let gen_bin_star psi1 psi2 = SSL.mk_star [psi1; psi2]
let gen_star psis = SSL.mk_star psis
let gen_septraction f g = SSL.mk_septraction f g

module type PARAMS = sig
  val n_vars : int
  val n_selectors : int * int
  val depth : int * int
  val lists : bool
  val unfold : bool
  val star_arity : int * int
end

module Make (Params : PARAMS) = struct

  let variables =
    List.range 1 `To Params.n_vars
    |> List.map (fun i -> Format.asprintf "x%d" i)

  let gen_var_name = QCheck.Gen.oneofl variables

  let gen_var = QCheck.Gen.map gen_var gen_var_name
  (*
  let gen_var_nil =
    QCheck.Gen.oneof [QCheck.Gen.map Variable.mk gen_var_name; fun _ _ -> Variable.nil]
  *)
  let gen_var_list =
    let min, max = Params.n_selectors in
    let gen_range = QCheck.Gen.int_range min max in
    QCheck.Gen.list_size gen_range gen_var

  let gen_atom = QCheck.Gen.oneof ([
    (*QCheck.Gen.map2 gen_eq gen_var gen_var;
    QCheck.Gen.map2 gen_neq gen_var gen_var;*)
    QCheck.Gen.map2 gen_pto gen_var gen_var;
    QCheck.Gen.map2 gen_pto gen_var gen_var;
    QCheck.Gen.map2 gen_pto gen_var gen_var;
    QCheck.Gen.map2 gen_pto gen_var gen_var;
  ] @ if Params.lists then [
    QCheck.Gen.map2 gen_ls gen_var gen_var
  ] else [])

  let qf_symbolic_heap =
    let min, max = Params.star_arity in
    let gen_range = QCheck.Gen.int_range min max in
    QCheck.Gen.list_size gen_range gen_atom
    |> QCheck.Gen.map gen_star

  let qf_symbolic_heap_entl =
    QCheck.Gen.map2 gen_gneg qf_symbolic_heap qf_symbolic_heap

  let bound_gen = QCheck.Gen.int_range (fst Params.depth) (snd Params.depth)

  let gen_formula = QCheck.Gen.(sized_size bound_gen @@ fix
    (fun self n -> match n with
      | 1 -> gen_atom
      | n ->
          if true then
          (QCheck.Gen.oneof [
              QCheck.Gen.map2 gen_and  (self (n-1)) (self (n-1));
              QCheck.Gen.map2 gen_bin_star (self (n-1)) (self (n-1));
              QCheck.Gen.map2 gen_gneg (self (n-1)) (self (n-1));
              QCheck.Gen.map2 gen_or (self (n-1)) (self (n-1));
              (*QCheck.Gen.map2 gen_septraction (self (n-1)) (self (n-1))*)
            ])
          else (QCheck.Gen.oneof [
            QCheck.Gen.map2 gen_and  (self (n-1)) (self (n-1));
            QCheck.Gen.map2 gen_or   (self (n-1)) (self (n-1));
            QCheck.Gen.map2 gen_bin_star (self (n-1)) (self (n-1));
            QCheck.Gen.map2 gen_gneg (self (n-1)) (self (n-1));
            (*QCheck.Gen.map2 gen_septraction (self (n-1)) (self (n-1))*)
          ])
    ))

  let entailment = QCheck.Gen.map2 gen_gneg gen_formula gen_formula

  (*
  let arbitrary_formula = QCheck.make @@ QCheck.Gen.map2 gen_gneg gen_formula gen_formula
  (*let arbitrary_formula = QCheck.make gen_formula*)

  let qf_symbolic_heap = QCheck.make @@
  *)

end
