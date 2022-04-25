(* Encoding of lists
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Z3

open Context
open Translation_sig

module Array = Z3.Z3Array

module List = BatList

module Make (Set : SET) (Locations : LOCATIONS) = struct

  let mk_succ (ctx : Context.t) x = Array.mk_select ctx.solver ctx.heap x

  let n_ary_select (ctx : Context.t) loc n =
    List.init n (fun _ -> 0)
    |> List.fold_left (fun expr _ -> mk_succ ctx expr) loc

  let mk_n_ary_succ_eq ctx x y n =
    let select = n_ary_select ctx x n in
    Boolean.mk_eq ctx.solver select y

  let allocated (ctx : Context.t) fp loc =
    Set.mk_mem ctx.solver loc fp

  let not_allocated (ctx : Context.t) fp loc =
    Boolean.mk_not ctx.solver (allocated ctx fp loc)

  (* ==== Auxiliary predicates ==== *)

  let reach_i ctx x y i = mk_n_ary_succ_eq ctx x y i

  let reach ctx x y (min, max) =
    List.range min `To ctx.bound
    |> List.map (reach_i ctx x y)
    |> Boolean.mk_or ctx.solver

  let all_reachable_i ctx fp x i =
    if i = 0 then Set.mk_eq_empty ctx.solver fp
    else
      List.range 0 `To (i - 1)
      |> List.map (n_ary_select ctx x)
      |> Set.mk_enumeration ctx.solver ctx.locs_sort
      |> Set.mk_eq ctx.solver fp

  let path_i ctx fp x y i =
    let phi1 = reach_i ctx x y i in
    let phi2 = all_reachable_i ctx fp x i in
    let phi3 = Boolean.mk_not ctx.solver (Set.mk_mem ctx.solver y fp) in
    Boolean.mk_and ctx.solver [phi1; phi2; phi3]

  let path ctx fp x y (min, max) =
    let paths =
      List.range min `To max
      |> List.map (path_i ctx fp x y)
      |> Boolean.mk_or ctx.solver
    in
    let no_path = Boolean.mk_not ctx.solver (reach ctx x y (min, max)) in
    let empty = Set.mk_eq_empty ctx.solver fp in
    let no_path_empty = Boolean.mk_and ctx.solver [no_path; empty] in
    Boolean.mk_or ctx.solver [paths; no_path_empty]

  (* ==== Encodings ==== *)

  module Classic = struct

    let semantics ctx fp x y bound = reach ctx x y bound

    let axioms ctx fp x y bound = path ctx fp x y bound

  end

  (** List encoding optimized for satisfiability of symbolic heaps *)
  module SymbolicHeaps = struct

    let semantics (ctx : Context.t) fp x y _ =
      let case1 = Boolean.mk_eq ctx.solver x y in
      let case2 = Boolean.mk_eq ctx.solver (mk_succ ctx x) y in
      Boolean.mk_or ctx.solver [case1; case2]

    let axioms (ctx : Context.t) fp x y _ =
      let cond1 = Boolean.mk_eq ctx.solver x y in
      let cond2 = Boolean.mk_eq ctx.solver (mk_succ ctx x) y in
      let fp1 = Set.mk_eq_empty ctx.solver fp in
      let fp2 = Set.mk_eq_singleton ctx.solver fp x in
      let case1 = Boolean.mk_implies ctx.solver cond1 fp1 in
      let case2 = Boolean.mk_implies ctx.solver cond2 fp2 in
      Boolean.mk_and ctx.solver [case1; case2]

  end

end
