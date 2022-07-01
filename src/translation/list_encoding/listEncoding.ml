(* Encoding of lists
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SMT

open Context
open Translation_sig

module List = BatList

(* TODO: remove functor *)
module Make (A : SET) (Locations : LOCATIONS) = struct

  let mk_succ ctx x = Array.mk_select ctx.heap x

  let mk_nth_succ ctx x n =
    List.init n (fun _ -> 0)
    |> List.fold_left (fun expr _ -> mk_succ ctx expr) x

  let mk_nth_succ_eq ctx x y n =
    let select = mk_nth_succ ctx x n in
    Boolean.mk_eq select y

  let mk_is_allocated fp loc = Set.mk_mem loc fp

  let mk_is_not_allocated fp loc = Boolean.mk_not (mk_is_allocated fp loc)

  (* ==== Auxiliary predicates ==== *)

  let reach_i ctx x y i = mk_nth_succ_eq ctx x y i

  let reach ctx x y (min, max) =
    List.range min `To ctx.bound
    |> List.map (reach_i ctx x y)
    |> Boolean.mk_or

  let all_reachable_i ctx fp x i =
    if i = 0 then Set.mk_eq_empty fp
    else
      List.range 0 `To (i - 1)
      |> List.map (mk_nth_succ ctx x)
      |> Set.mk_enumeration ctx.fp_sort
      |> Set.mk_eq fp

  let path_i ctx fp x y i =
    let phi1 = reach_i ctx x y i in
    let phi2 = all_reachable_i ctx fp x i in
    let phi3 = Boolean.mk_not (Set.mk_mem y fp) in
    Boolean.mk_and [phi1; phi2; phi3]

  let path ctx fp x y (min, max) =
    (*let x_var = SSL.Variable.mk @@ Z3.Expr.to_string x in
    let y_var = SSL.Variable.mk @@ Z3.Expr.to_string y in
    let pointers, lists = SL_graph.predict_footprint ctx.sl_graph x_var y_var in
    if not @@ ctx.polarity && (List.length lists) > 0 then
      let ptrs =
        pointers
        |> List.map (fun phi -> match phi with SSL.PointsTo (x, y) -> x)
        |> List.map (Locations.var_to_expr ctx)
        |> Set.mk_enumeration ctx.solver ctx.locs_sort
      in
      let lists = List.map
        (fun ls ->
          let id = formula_footprint ~physically:false ctx ls in
          Printf.printf "%s: %s\n" (SSL.show ls) id;
          Set.mk_const_s ctx.solver id ctx.locs_sort
        ) lists
      in
      let fp_def = Set.mk_union ctx.solver (ptrs :: lists) in
      Printf.printf "ls(%s, %s) ~> %s\n" (Expr.to_string x) (Expr.to_string y) (Expr.to_string fp_def);
      let equal = Boolean.mk_eq ctx.solver x y in
      let fp_empty = Set.mk_eq_empty ctx.solver fp in
      let fp_not_empty = Set.mk_eq ctx.solver fp fp_def in
      Boolean.mk_ite ctx.solver equal fp_empty fp_not_empty

    else*)
      let paths =
        List.range min `To max
        |> List.map (path_i ctx fp x y)
        |> Boolean.mk_or
      in
      let no_path = Boolean.mk_not (reach ctx x y (min, max)) in
      let empty = Set.mk_eq_empty fp in
      let no_path_empty = Boolean.mk_and [no_path; empty] in
      Boolean.mk_or [paths; no_path_empty]

  (* ==== Encodings ==== *)

  module Classic = struct

    let semantics ctx fp x y bound = reach ctx x y bound

    let axioms ctx fp x y bound = path ctx fp x y bound

  end


  (** List encoding optimized for satisfiability of symbolic heaps *)
  module SymbolicHeaps = struct

    let semantics (ctx : Context.t) fp x y _ =
      let case1 = Boolean.mk_eq x y in
      let case2 = Boolean.mk_eq (mk_succ ctx x) y in
      Boolean.mk_or [case1; case2]

    let axioms (ctx : Context.t) fp x y _ =
      let cond1 = Boolean.mk_eq x y in
      let cond2 = Boolean.mk_and [
        (Boolean.mk_eq (mk_succ ctx x) y);
        (Boolean.mk_not @@ Boolean.mk_eq x y)
      ]
      in
      let fp1 = Set.mk_eq_empty fp in
      let fp2 = Set.mk_eq_singleton fp x in
      let case1 = Boolean.mk_implies cond1 fp1 in
      let case2 = Boolean.mk_implies cond2 fp2 in
      Boolean.mk_and [case1; case2]

  end

end
