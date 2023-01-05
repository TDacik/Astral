open SMT
open Translation_context
open ReachabilityEncoding

let semantics_empty ctx dom fp1 fp2 x y bound =
  Boolean.mk_and [
    Boolean.mk_eq x y;
    Set.mk_eq_empty dom;
  ]

let semantics_non_empty ctx dom fp1 fp2 x y bound =
  let reach1 = reach ctx x y bound in
  let reach2 = reach {ctx with heap = ctx.heap_prev} x y bound in
  let domain_def = Set.mk_eq dom fp1 in
  let path_inclusion = Set.mk_subset fp2 fp2 in
  Boolean.mk_and [
    Boolean.mk_neq x y;
    reach1;
    reach2;
    domain_def;
    path_inclusion;
  ]

let semantics ctx dom fp1 fp2 x y bound =
  Boolean.mk_or [
    semantics_empty ctx dom fp1 fp2 x y bound;
    semantics_non_empty ctx dom fp1 fp2 x y bound
  ]

(** The footprint of doubly-linked list is the domain of the path from x to l. *)
let axioms ctx fp1 fp2 x y bound =
  let path_def1 = path ctx fp1 x y bound in
  let path_def2 = path {ctx with heap = ctx.heap_prev} fp2 x y bound in
  Boolean.mk_and [path_def1; path_def2]
