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

  let mk_const sort str = Var.mk str sort

  let var_to_expr context var = Var.mk (SSL.Variable.show var) context.locs_sort

  let vars_to_exprs context = List.map (var_to_expr context) context.vars

  let enumeration sort = Enumeration.get_constants sort

  let quantify sort quantifier expr_constructor =
    let binder_e = Var.mk_fresh "l" sort in
    let expr = expr_constructor binder_e in
    match quantifier with
    | `Forall -> Quantifier.mk_forall binder_e expr
    | `Exists -> Quantifier.mk_exists binder_e expr

  let quantify2 sort quantifier expr_constructor =
    let binder_e = Var.mk_fresh "L" sort in
    let expr = expr_constructor binder_e in
    match quantifier with
    | `Forall2 -> Quantifier.mk_forall binder_e expr
    | `Exists2 -> Quantifier.mk_exists binder_e expr

  let exists ctx constructor = quantify ctx.locs_sort `Exists constructor

  let forall ctx constructor = quantify ctx.locs_sort `Forall constructor

  let exists2 ctx constructor = quantify2 ctx.fp_sort `Exists2 constructor

  let forall2 ctx constructor = quantify2 ctx.fp_sort `Forall2 constructor

  (* ==== Location lemmas ==== *)

  (* Convention: if phi has n variables, locations n+1, ... are anonymous
  let annonymous context =
    Enumeration.get_constants context.locs_sort
    |> List.drop (List.length context.vars)

  let location_lemmas context =
    let annonymous = annonymous context in

    let definition = List.map
      (fun var ->
        Boolean.mk_distinct (var :: annonymous)
      ) (vars_to_exprs context)
      |> Boolean.mk_and
    in
    let no_annon_succ = List.map
      (fun loc ->
        let succ = Z3Array.mk_select context.heap loc in
        Boolean.mk_distinct (loc :: annonymous)
      ) (vars_to_exprs context)
      |> Boolean.mk_and
    in
    Boolean.mk_and [definition; no_annon_succ]*)
  let location_lemmas ctx = Boolean.mk_true ()

end
