(* Separation logic.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open MemoryModel

open BaseLogic_terms
open BaseLogic_utils

module Boolean = BaseLogic_boolean.Smart (* TODO: split *)

module SeparationLogic = struct

  module ID = Identifier.Make ()

  let nil = of_var @@ Variable.nil

  let mk_block_begin x = mk_app BlockBegin [x]

  let mk_block_end x = mk_app BlockEnd [x]

  let mk_heap_term field source = mk_app (HeapTerm field) [source]

  let mk_pure phi =
    assert (Sort.is_bool @@ get_sort phi);
    mk_app Pure [phi]

  let emp = mk_app Emp []

  let mk_star psis = mk_smart_app Star ~neutral:emp ~anihilator:Boolean.ff psis

  let mk_septraction lhs rhs = mk_app Septraction [lhs; rhs]
  let mk_wand lhs rhs = Boolean.mk_not @@ mk_septraction lhs (Boolean.mk_not rhs)

  let mk_pto_struct x s ys =
    let sorts = List.map Field.get_sort @@ StructDef.get_fields s in
    let what = Format.asprintf "constructor %s" (StructDef.get_constructor s) in
    check_types ~what sorts ys;
    let rhs = mk_app (Constructor s) ys in
    mk_app PointsTo [x; rhs]

  let mk_pto_tuple x ys =
    let struct_def = StructDef.mk_tuple @@ List.length ys in
    let rhs = mk_app (Constructor struct_def) ys in
    mk_app PointsTo [x; rhs]

  let mk_pto x y = mk_pto_struct x StructDef.ls [y]

  (*
  let mk_pto_dls x n p = mk_pto_struct x StructDef.dls [n; p]
  let mk_pto_nls x n t = mk_pto_struct x StructDef.nls [n; t]
  *)

  let mk_predicate name ?(structs=[]) xs = mk_app (Predicate (ID.mk name, structs)) xs
  let mk_ls x y = mk_predicate "ls" [x; y]
  let mk_dls x y f l = mk_predicate "dls" [x; y; f; l]
  let mk_nls x y z = mk_predicate "nls" [x; y; z]

  let mk_gneg lhs rhs =
    if !BaseLogic_config.do_simplification && lhs === rhs then Boolean.ff
    else mk_app GuardedNot [lhs; rhs]

  let is_nil x = equal x nil

end
