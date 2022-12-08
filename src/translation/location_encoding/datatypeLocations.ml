(* Encoding of locations using datatype with constant constructors.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT

include Enumeration

type t = Sort.t

let name = "datatypes"

let mk str n =
  let names = List.map (fun x -> Format.asprintf "%d" x) (BatList.range 1 `To n) in
  Enumeration.mk_sort str names

let get_sort sort = sort

let var_axiom _ var = Boolean.mk_true ()

let quantify sort quantifier expr_constructor =
  let binder_e = Variable.mk_fresh "l" sort in
  let expr = expr_constructor binder_e in
  match quantifier with
  | `Forall -> Quantifier.mk_forall [binder_e] expr
  | `Exists -> Quantifier.mk_exists [binder_e] expr

let mk_forall sort constructor = quantify sort `Forall constructor
let mk_exists sort constructor = quantify sort `Exists constructor

let location_lemmas sort = Boolean.mk_true ()
