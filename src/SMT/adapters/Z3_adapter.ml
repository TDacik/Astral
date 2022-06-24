(* Z3 adapter for Astral
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

type t = Z3.Expr.expr
type model = Z3.Model.model

let context = ref (Z3.mk_context [])

(** Translation of SMT expressions *)
let rec translate = function
  | SMT.Constant name -> failwith "translation of constant not implemented"
  | SMT.Variable (x, sort) -> Z3.Expr.mk_const_s !context x (translate_sort sort)

  | SMT.True -> Z3.Boolean.mk_true !context
  | SMT.False -> Z3.Boolean.mk_false !context
  | SMT.Equal (t1, t2) -> Z3.Boolean.mk_eq !context (translate t1) (translate t2)
  | SMT.Distinct ts -> Z3.Boolean.mk_distinct !context (List.map translate ts)
  | SMT.And es -> Z3.Boolean.mk_and !context (List.map translate es)
  | SMT.Or es -> Z3.Boolean.mk_or !context (List.map translate es)
  | SMT.Not e -> Z3.Boolean.mk_not !context (translate e)
  | SMT.Implies (e1, e2) -> Z3.Boolean.mk_implies !context (translate e1) (translate e2)
  | SMT.Iff (e1, e2) -> Z3.Boolean.mk_iff !context (translate e1) (translate e2)

  | SMT.Membership (x, s) -> Z3.Set.mk_membership !context (translate x) (translate s)
  | SMT.Subset (s1, s2) ->
      let _ = Format.printf "%s\n" (SMT.Term.to_string s1) in
      let _ = Format.printf "%s : %s\n" (Z3.Expr.to_string (translate s1)) (Z3.Sort.to_string @@ Z3.Expr.get_sort (translate s1)) in
      Z3.Set.mk_subset !context (translate s1) (translate s2)
  | SMT.Union (sets, _) -> Z3.Set.mk_union !context (List.map translate sets)
  | SMT.Inter (sets, _) -> Z3.Set.mk_intersection !context (List.map translate sets)
  | SMT.Diff (s1, s2) -> Z3.Set.mk_difference !context (translate s1) (translate s2)
  | SMT.Compl s -> Z3.Set.mk_complement !context (translate s)

  | SMT.Disjoint (s1, s2) ->
      let intersect = Z3.Set.mk_intersection !context [translate s1; translate s2] in
      let sort = translate_sort @@ SMT.Set.get_elem_sort s1 in
      let empty = Z3.Set.mk_empty !context sort in
      Z3.Boolean.mk_eq !context empty intersect

  | SMT.Enumeration (sets, sort) ->
      let empty = Z3.Set.mk_empty !context (translate_sort (SMT.Sort.get_elem_sort sort)) in
      List.fold_left (Z3.Set.mk_set_add !context) empty (List.map translate sets)

  | SMT.ConstArr const ->
      Z3.Z3Array.mk_const_array !context (Z3.Expr.get_sort @@ translate const) (translate const)
  | SMT.Select (a, i) -> Z3.Z3Array.mk_select !context (translate a) (translate i)
  | SMT.Store (a, i, v) -> Z3.Z3Array.mk_store !context (translate a) (translate i) (translate v)

  | SMT.Exists (x, phi) ->
      let binder_e = translate x in
      Z3.Quantifier.mk_exists_const !context [binder_e] (translate phi) None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier

  | SMT.Forall (x, phi) ->
      let binder_e = translate x in
      Z3.Quantifier.mk_forall_const !context [binder_e] (translate phi) None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier

and translate_sort = function
  | SMT.Bool -> Z3.Boolean.mk_sort !context
  | SMT.Integer -> Z3.Arithmetic.Integer.mk_sort !context
  | SMT.Finite (name, consts) ->
      Z3.Enumeration.mk_sort_s !context name (List.map SMT.Term.to_string consts)
  | SMT.Set (elem_sort) -> Z3.Set.mk_sort !context (translate_sort elem_sort)
  | SMT.Array (d, r) -> Z3.Z3Array.mk_sort !context (translate_sort d) (translate_sort r)

let solve (phi : SMT.term) =
  let solver = Z3.Solver.mk_simple_solver !context in
  Printf.printf "%s\n" (Z3.SMT.benchmark_to_smtstring !context
    "Input for solver"
    "ALL"
    "unknown"
    ""
    []
    (translate phi)
  );
  match Z3.Solver.check solver [translate phi] with
  | Z3.Solver.SATISFIABLE -> SMT.SMT_Sat
  | Z3.Solver.UNSATISFIABLE -> SMT.SMT_Unsat
  | Z3.Solver.UNKNOWN -> SMT.SMT_Unknown

let evaluate term model = None
