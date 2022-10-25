(* Encoding of locations using finite sort and quantifiers.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT

open Context
open Set_sig

module List = BatList

module Locations (Set : SET) = struct

  let mk_sort str n =
    let names =
      List.map (fun x -> Format.asprintf "%d" x) (List.range 1 `To n)
    in
    Enumeration.mk_sort str names

  let mk_const sort str = SMT.Variable.mk str sort

  let var_to_expr context var = SMT.Variable.mk (SSL.Variable.show var) context.locs_sort

  let vars_to_exprs context = List.map (var_to_expr context) context.vars

  let enumeration sort = Enumeration.get_constants sort

  let quantify sort quantifier expr_constructor =
    let binder_e = SMT.Variable.mk_fresh "l" sort in
    let expr = expr_constructor binder_e in
    match quantifier with
    | `Forall -> Quantifier.mk_forall binder_e expr
    | `Exists -> Quantifier.mk_exists binder_e expr

  let quantify2 sort quantifier expr_constructor =
    let binder_e = SMT.Variable.mk_fresh "L" sort in
    let expr = expr_constructor binder_e in
    match quantifier with
    | `Forall2 -> Quantifier.mk_forall binder_e expr
    | `Exists2 -> Quantifier.mk_exists binder_e expr

  let exists ctx constructor = quantify ctx.locs_sort `Exists constructor

  let forall ctx constructor = quantify ctx.locs_sort `Forall constructor

  let exists2 ctx constructor = quantify2 ctx.fp_sort `Exists2 constructor

  let forall2 ctx constructor = quantify2 ctx.fp_sort `Forall2 constructor

  (* ==== Location lemmas ==== *)

  (* Convention: if phi has n variables, locations n+1, ... are anonymous *)
  let annonymous context =
    Enumeration.get_constants context.locs_sort
    |> List.drop (List.length context.vars)

  let location_lemmas context = Boolean.mk_true ()
    (*let annonymous = annonymous context in
    let named = vars_to_exprs context in

    let no_annon_succ = List.map
      (fun loc ->
        let succ = Array.mk_select context.heap loc in
        Boolean.mk_distinct (succ :: annonymous)
      ) annonymous
      |> Boolean.mk_and
    in

    Boolean.mk_and [no_annon_succ]
    *)
end
