(* Generator of random formulae
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open Batteries

open QCheck.Gen

(* === Base generators === *)

let gen_var var = SSL.mk_var var
let gen_eq x y = SSL.mk_eq x y
let gen_neq x y = SSL.mk_distinct x y
let gen_pt x ys = SSL.mk_pto_seq x ys
let gen_ls x y = SSL.mk_ls x y

let gen_and f g = SSL.mk_and [f; g]
let gen_or f g = SSL.mk_or [f; g]
let gen_gneg f g = SSL.mk_gneg f g
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

  let gen_var_nil =
    QCheck.Gen.oneof [QCheck.Gen.map Variable.mk gen_var_name; fun _ -> Variable.nil]

  let gen_var_list =
    let min, max = Params.n_selectors in
    let gen_range = QCheck.Gen.int_range min max in
    QCheck.Gen.list_size gen_range gen_var

  let gen_atom = QCheck.Gen.oneof ([
    QCheck.Gen.map2 gen_eq gen_var gen_var;
    QCheck.Gen.map2 gen_neq gen_var gen_var;
    QCheck.Gen.map2 gen_pt gen_var gen_var_list;
  ] @ if Params.lists then [
    QCheck.Gen.map2 gen_ls gen_var gen_var
  ] else [])

  let gen_qf_symbolic_heap =
    let min, max = Params.star_arity in
    let gen_range = QCheck.Gen.int_range min max in
    QCheck.Gen.list_size gen_range gen_atom
    |> QCheck.Gen.map gen_star

  let gen_qf_symbolic_heap_entl =
    QCheck.Gen.map2 gen_gneg gen_qf_symbolic_heap gen_qf_symbolic_heap

  let qf_symbolic_heap =
    QCheck.make ~print:SSL.show gen_qf_symbolic_heap

  let qf_symbolic_heap_entl =
    QCheck.make ~print:SSL.show gen_qf_symbolic_heap_entl

  (*
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

  let qf_symbolic_heap = QCheck.make @@
  *)

end
