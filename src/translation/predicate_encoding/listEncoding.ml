(* Encoding of lists
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SMT
open ReachabilityEncoding

module Classic = struct

  let semantics ctx fp x y bound =
    let reach = reach ctx x y bound in
    Boolean.mk_and [reach; path ctx fp x y bound]

  let axioms ctx fp x y bound = path ctx fp x y bound

end

(** List encoding optimized for satisfiability of symbolic heaps *)
module SymbolicHeaps = struct

  let semantics ctx fp x y _ =
    let cond1 = Boolean.mk_eq x y in
    let cond2 = Boolean.mk_and [
      Boolean.mk_distinct x y;
      Boolean.mk_eq (mk_succ ctx x) y;
    ]
    in
    let case1 = Boolean.mk_and [cond1; Set.mk_eq_empty fp] in
    let case2 = Boolean.mk_and [cond2; Set.mk_eq_singleton fp x ] in
    Boolean.mk_or [case1; case2]

  let axioms _ _ _ _ _ = Boolean.mk_true ()

end
