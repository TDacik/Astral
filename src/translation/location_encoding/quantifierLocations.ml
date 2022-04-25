(* Encoding of locations using finite sort and quantifiers.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Z3

open Context
open Set_sig

module List = BatList

module Locations (Set : SET) = struct

  let mk_sort_s context str n =
    let names = List.map (Format.asprintf "%d") (List.range 1 `To n) in
    Enumeration.mk_sort_s context str names

  let mk_const_s context sort str = Expr.mk_const_s context str sort

  let var_to_expr context var =
    mk_const_s context.solver context.locs_sort (SSL.Variable.show var)

  let vars_to_exprs context = List.map (var_to_expr context) context.vars

  let enumeration sort = Enumeration.get_consts sort

  (* Convention: if phi has n variables, locations n+1, ... all anonymous *)
  let annonymous context =
    Enumeration.get_consts context.locs_sort
    |> List.drop (List.length context.vars)

  let quantify context sort quantifier expr_constructor =
    let binder_e = Expr.mk_fresh_const context "l" sort in
    let expr = expr_constructor binder_e in
    let q = match quantifier with
    | `Forall -> Quantifier.mk_forall_const context [binder_e] expr None [] [] None None
    | `Exists -> Quantifier.mk_exists_const context [binder_e] expr None [] [] None None
    in
    Quantifier.expr_of_quantifier q

  let quantify2 context sort quantifier expr_constructor =
    let binder_e = Expr.mk_fresh_const context "L" sort in
    let expr = expr_constructor binder_e in
    let q = match quantifier with
    | `Forall2 -> Quantifier.mk_forall_const context [binder_e] expr None [] [] None None
    | `Exists2 -> Quantifier.mk_exists_const context [binder_e] expr None [] [] None None
    in
    Quantifier.expr_of_quantifier q

  let exists ctx constructor = quantify ctx.solver ctx.locs_sort `Exists constructor

  let forall ctx constructor = quantify ctx.solver ctx.locs_sort `Forall constructor

  let exists2 ctx constructor = quantify2 ctx.solver ctx.footprint_sort `Exists2 constructor

  let forall2 ctx constructor = quantify2 ctx.solver ctx.footprint_sort `Forall2 constructor

  let location_lemmas context =
    let annonymous = annonymous context in

    let definition = List.map
      (fun var ->
        Boolean.mk_distinct context.solver (var :: annonymous)
      ) (vars_to_exprs context)
      |> Boolean.mk_and context.solver
    in
    let no_annon_succ = List.map
      (fun loc ->
        let succ = Z3Array.mk_select context.solver context.heap loc in
        Boolean.mk_distinct context.solver (loc :: annonymous)
      ) (vars_to_exprs context)
      |> Boolean.mk_and context.solver
    in
    (*Boolean.mk_and context.solver [definition; no_annon_succ]*)
    let distinct = Boolean.mk_distinct context.solver (enumeration context.locs_sort) in
    Boolean.mk_and context.solver [no_annon_succ; distinct]

end
