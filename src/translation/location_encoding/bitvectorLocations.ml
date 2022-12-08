(* Encoding of locations using bitvectors.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT

include Bitvector

let name = "bitvectors"

type t = SMT.Sort.t * int
(* Sort augmented with bound constraint *)

(** Compute minimal width of sort for n locations. *)
let compute_width n =
  let log2 = (fun x -> Float.div (Float.log x) (Float.log @@ Float.of_int 2)) in
  let res =
    log2 (Float.of_int n)
    |> Float.ceil
    |> Float.to_int
  in
  if res == 0 then 1 else res

let mk str n =
  (* TODO: make the width lesser? *)
  (Bitvector.mk_sort n, n)

let get_sort (sort, n) = sort

let get_constants (_, n) =
  List.map (fun i -> Bitvector.mk_const i n) (BatList.range 0 `To n)

let var_axiom (_, n) var =
  Bitvector.mk_lesser_eq var (Bitvector.mk_const (n-1) (Bitvector.get_width var))

let quantify sort quantifier expr_constructor =
  let binder_e = Variable.mk_fresh "l" sort in
  let expr = expr_constructor binder_e in
  match quantifier with
  | `Forall -> Quantifier.mk_forall [binder_e] expr
  | `Exists -> Quantifier.mk_exists [binder_e] expr

let mk_forall sort constructor = quantify sort `Forall constructor
let mk_exists sort constructor = quantify sort `Exists constructor

let location_lemmas _ = Boolean.mk_true ()
