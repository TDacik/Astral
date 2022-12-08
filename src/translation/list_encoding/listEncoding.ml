(* Encoding of lists
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SMT

open Context

module List = BatList

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
  List.range min `To ctx.location_bound
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

  let semantics ctx fp x y bound =
    let reach = reach ctx x y bound in
    Boolean.mk_and [reach; path ctx fp x y bound]

  let axioms ctx fp x y bound = path ctx fp x y bound

end

(** List encoding optimized for satisfiability of symbolic heaps *)
module SymbolicHeaps = struct

  let semantics (ctx : Context.t) fp x y _ =
    let cond1 = Boolean.mk_eq x y in
    let cond2 = Boolean.mk_and [
      Boolean.mk_distinct [x; y];
      Boolean.mk_eq (mk_succ ctx x) y;
    ]
    in
    let case1 = Boolean.mk_and [cond1; Set.mk_eq_empty fp] in
    let case2 = Boolean.mk_and [cond2; Set.mk_eq_singleton fp x ] in
    Boolean.mk_or [case1; case2]

  let axioms _ _ _ _ _ = Boolean.mk_true ()

end
