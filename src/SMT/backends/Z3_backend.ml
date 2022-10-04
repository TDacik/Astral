(* Z3 adapter for Astral
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Solver_utils

(* === Declarations === *)

type formula = Z3.Expr.expr

type model = Z3.Model.model

type status =
 | SMT_Sat of model
 | SMT_Unsat of SMT.Term.t list
 | SMT_Unknown of string

let name = "Z3"

(* === Initialization === *)

let context = ref (Z3.mk_context [])

let solver = ref (Z3.Solver.mk_simple_solver !context)

let is_available () = true

let init () =
  context := Z3.mk_context [];
  solver := Z3.Solver.mk_simple_solver !context

(* === Translation === *)

let rec translate t = match t with
  | SMT.Constant (name, sort) -> Z3.Expr.mk_const_s !context name (translate_sort sort)
  | SMT.Variable (x, sort, d) ->
      let name = SMT.Term.show t in
      Z3.Expr.mk_const_s !context name (translate_sort sort)

  | SMT.True -> Z3.Boolean.mk_true !context
  | SMT.False -> Z3.Boolean.mk_false !context
  | SMT.Equal (t1, t2) -> Z3.Boolean.mk_eq !context (translate t1) (translate t2)
  | SMT.Distinct ts -> Z3.Boolean.mk_distinct !context (List.map translate ts)
  | SMT.And es ->
      begin try Z3.Boolean.mk_and !context (List.map translate es)
      with e -> let _ = Printf.printf "%s" (SMT.Term.show (SMT.And es)) in raise e end
  | SMT.Or es -> Z3.Boolean.mk_or !context (List.map translate es)
  | SMT.Not e -> Z3.Boolean.mk_not !context (translate e)
  | SMT.Implies (e1, e2) -> Z3.Boolean.mk_implies !context (translate e1) (translate e2)
  | SMT.Iff (e1, e2) -> Z3.Boolean.mk_iff !context (translate e1) (translate e2)

  | SMT.Membership (x, s) -> Z3.Set.mk_membership !context (translate x) (translate s)
  | SMT.Subset (s1, s2) -> Z3.Set.mk_subset !context (translate s1) (translate s2)
  | SMT.Union (sets, _) -> Z3.Set.mk_union !context (List.map translate sets)
  | SMT.Inter (sets, _) ->
      begin
        try Z3.Set.mk_intersection !context (List.map translate sets)
        with e ->
          let _ = Printf.printf "===\nErr: %s\n===\n" (SMT.Term.show @@ List.hd sets) in
          raise e
      end
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


  | SMT.IntConst i -> Z3.Arithmetic.Integer.mk_numeral_i !context i
  | SMT.Plus (e1, e2) -> Z3.Arithmetic.mk_add !context [translate e1; translate e2]
  | SMT.Minus (e1, e2) -> Z3.Arithmetic.mk_sub !context [translate e1; translate e2]
  | SMT.Mult (e1, e2) -> Z3.Arithmetic.mk_mul !context [translate e1; translate e2]

  | SMT.Exists (x, phi) ->
      let binder_e = translate x in
      Z3.Quantifier.mk_exists_const !context [binder_e] (translate phi) None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier

  | SMT.Forall (x, phi) ->
      let binder_e = translate x in
      Z3.Quantifier.mk_forall_const !context [binder_e] (translate phi) None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier

and translate_sort = function
  | SMT.Sort.Bool -> Z3.Boolean.mk_sort !context
  | SMT.Sort.Integer -> Z3.Arithmetic.Integer.mk_sort !context
  | SMT.Sort.Finite (name, cs) -> Z3.Enumeration.mk_sort_s !context name cs
  | SMT.Sort.Set (elem_sort) -> Z3.Set.mk_sort !context (translate_sort elem_sort)
  | SMT.Sort.Array (d, r) -> Z3.Z3Array.mk_sort !context (translate_sort d) (translate_sort r)

(* === Solver === *)

let solve phi =
  let phi = translate phi in
  match Z3.Solver.check !solver [phi] with
  | Z3.Solver.SATISFIABLE ->
      let model = Option.get @@ Z3.Solver.get_model !solver in
      SMT_Sat model

  (* TODO: unsat core *)
  | Z3.Solver.UNSATISFIABLE ->
      let unsat_core = [] in
      SMT_Unsat unsat_core

  | Z3.Solver.UNKNOWN ->
      let reason = Z3.Solver.get_reason_unknown !solver in
      SMT_Unknown reason

let simplify phi = Z3.Expr.simplify phi None

(* === Model manipulation === *)

let rec inverse_translate model sort expr = match sort with
  | SMT.Sort.Finite _ -> SMT.Enumeration.mk_const sort (Z3.Expr.to_string expr)
  | SMT.Sort.Set _ ->
    let elem_sort = Z3.Z3Array.get_domain @@ Z3.Expr.get_sort expr in
    let elems = Z3.Enumeration.get_consts elem_sort in
    List.fold_left
    (fun acc elem ->
      match Z3.Model.eval model (Z3.Set.mk_membership !context elem expr) false with
      | Some res ->
          if Z3.Boolean.is_true res then elem :: acc
          else acc
      | None -> failwith "Internal error"
    ) [] elems
    |> List.map (inverse_translate model (SMT.Sort.get_elem_sort sort))
    |> SMT.Set.mk_enumeration (SMT.Sort.get_elem_sort sort)

  | _ -> failwith ("Cannot convert Z3 expression:" ^ Z3.Expr.to_string expr)

let eval model term =
  try
    let sort = SMT.Term.get_sort term in
    inverse_translate model sort @@ Option.get @@ Z3.Model.eval model (translate term) true
  with Invalid_argument _ | Z3.Error _ -> raise Evaluation_failed


(* === Debugging === *)

let show_formula phi = Z3.Expr.to_string phi
let show_model model = Z3.Model.to_string model
