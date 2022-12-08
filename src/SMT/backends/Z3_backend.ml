(* Z3 adapter for Astral
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

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
  | SMT.Variable (name, sort) -> Z3.Expr.mk_const_s !context name (translate_sort sort)

  | SMT.True -> Z3.Boolean.mk_true !context
  | SMT.False -> Z3.Boolean.mk_false !context
  | SMT.Equal (t1, t2) ->
      begin
        try Z3.Boolean.mk_eq !context (translate t1) (translate t2)
        with _ ->
          Format.printf "%s = %s"
            (SMT.Term.show_with_sort t1)
            (SMT.Term.show_with_sort t2)
          ; failwith "FUCK"
        end
  | SMT.Distinct ts -> Z3.Boolean.mk_distinct !context (List.map translate ts)
  | SMT.And es -> Z3.Boolean.mk_and !context (List.map translate es)
  | SMT.Or es -> Z3.Boolean.mk_or !context (List.map translate es)
  | SMT.Not e -> Z3.Boolean.mk_not !context (translate e)
  | SMT.Implies (e1, e2) -> Z3.Boolean.mk_implies !context (translate e1) (translate e2)
  | SMT.Iff (e1, e2) -> Z3.Boolean.mk_iff !context (translate e1) (translate e2)

  | SMT.LesserEq (e1, e2) -> begin match SMT.Term.get_sort e1 with
    | SMT.Sort.Bitvector _ -> Z3.BitVector.mk_ule !context (translate e1) (translate e2)
    (* TODO: other sorts *)
  end
  | SMT.Membership (x, s) -> Z3.Set.mk_membership !context (translate x) (translate s)
  | SMT.Subset (s1, s2) -> Z3.Set.mk_subset !context (translate s1) (translate s2)
  | SMT.Union (sets, _) -> Z3.Set.mk_union !context (List.map translate sets)
  | SMT.Inter (sets, _) -> Z3.Set.mk_intersection !context (List.map translate sets)
  | SMT.Diff (s1, s2) -> Z3.Set.mk_difference !context (translate s1) (translate s2)
  | SMT.Compl s -> Z3.Set.mk_complement !context (translate s)

  (* Bitvectors *)
  | SMT.BitConst (number, width) ->
      Z3.BitVector.mk_numeral !context (string_of_int number) width
  | SMT.BitCheck (bv, index) ->
      let width = SMT.Bitvector.get_width bv in
      let one = Z3.BitVector.mk_numeral !context "1" width in
      let index_expr = translate index in
      let bv_expr = translate bv in
      let mask = Z3.BitVector.mk_shl !context one index_expr in
      let app = Z3.BitVector.mk_and !context bv_expr mask in
      let zero = Z3.BitVector.mk_numeral !context "0" width in
      Z3.Boolean.mk_distinct !context [app; zero]

  | SMT.BitAnd (bvs, SMT.Sort.Bitvector n) ->
      let ones = Z3.BitVector.mk_repeat !context n (Z3.BitVector.mk_numeral !context "1" 1) in
      List.fold_left (fun acc bv -> Z3.BitVector.mk_and !context acc (translate bv)) ones bvs

  | SMT.BitOr (bvs, SMT.Sort.Bitvector n) ->
      let zeros = Z3.BitVector.mk_numeral !context "0" n in
      List.fold_left (fun acc bv -> Z3.BitVector.mk_or !context acc (translate bv)) zeros bvs

  | SMT.BitXor ([bv1; bv2], sort) -> Z3.BitVector.mk_xor !context (translate bv1) (translate bv2)
  | SMT.BitImplies (bv1, bv2) ->
      Z3.BitVector.mk_or !context (Z3.BitVector.mk_not !context (translate bv1)) (translate bv2)
  | SMT.BitCompl bv -> Z3.BitVector.mk_not !context (translate bv)
  | SMT.BitShiftLeft (bv, rotate) ->
      Z3.BitVector.mk_shl !context (translate bv) (translate rotate)
  | SMT.BitShiftRight (bv, rotate) ->
      Z3.BitVector.mk_lshr !context (translate bv) (translate rotate)

  | SMT.Disjoint (s1, s2) ->
      let intersect = Z3.Set.mk_intersection !context [translate s1; translate s2] in
      let sort = translate_sort @@ SMT.Set.get_elem_sort s1 in
      let empty = Z3.Set.mk_empty !context sort in
      Z3.Boolean.mk_eq !context empty intersect

  | SMT.Enumeration (sets, sort) ->
      let empty = Z3.Set.mk_empty !context (translate_sort (SMT.Sort.get_elem_sort sort)) in
      List.fold_left (Z3.Set.mk_set_add !context) empty (List.map translate sets)

  | SMT.ConstArr (const, _) ->
      Z3.Z3Array.mk_const_array !context (Z3.Expr.get_sort @@ translate const) (translate const)
  | SMT.Select (a, i) -> Z3.Z3Array.mk_select !context (translate a) (translate i)
  | SMT.Store (a, i, v) -> Z3.Z3Array.mk_store !context (translate a) (translate i) (translate v)

  | SMT.IntConst i -> Z3.Arithmetic.Integer.mk_numeral_i !context i
  | SMT.Plus (e1, e2) -> Z3.Arithmetic.mk_add !context [translate e1; translate e2]
  | SMT.Minus (e1, e2) -> Z3.Arithmetic.mk_sub !context [translate e1; translate e2]
  | SMT.Mult (e1, e2) -> Z3.Arithmetic.mk_mul !context [translate e1; translate e2]

  | SMT.Exists (xs, phi) ->
      let binders = List.map translate xs in
      Z3.Quantifier.mk_exists_const !context binders (translate phi) None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier

  | SMT.Forall (xs, phi) ->
      let binders = List.map translate xs in
      Z3.Quantifier.mk_forall_const !context binders (translate phi) None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier

  | SMT.Forall2 _ | SMT.Exists2 _ ->
      failwith "Internal error: second order quantification should be removed before \
                translating to backend solver"

  | other -> failwith (Format.asprintf "Z3 wrapper error at: %s" (SMT.Term.show other))


and translate_sort = function
  | SMT.Sort.Bool -> Z3.Boolean.mk_sort !context
  | SMT.Sort.Int -> Z3.Arithmetic.Integer.mk_sort !context
  | SMT.Sort.Bitvector n -> Z3.BitVector.mk_sort !context n
  | SMT.Sort.Finite (name, constants) -> Z3.Enumeration.mk_sort_s !context name constants
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
(*
let rec inverse_translate_set_array model expr elem_sort =
  (* We assume that the element sort is always finite enumeration. *)
  let elems =
    SMT.Enumeration.get_constants elem_sort
    |> List.map translate
  in
  List.fold_left
  (fun acc elem ->
    match Z3.Model.eval model (Z3.Set.mk_membership !context elem expr) false with
    | Some res ->
        if Z3.Boolean.is_true res then elem :: acc
        else acc
    | None -> failwith "Internal error"
  ) [] elems
  |> List.map (inverse_translate model elem_sort)
  |> SMT.Set.mk_enumeration elem_sort
*)
(** Translation of Z3's term representation to SMT.

    @param model   Z3 model
    @param expr    Z3 expression to be translated
    @param sort    SMT sort of the expression

and inverse_translate model sort expr = match sort with
  | SMT.Sort.Finite _ -> SMT.Enumeration.mk_const sort (Z3.Expr.to_string expr)

  (* SMT sets can be represented either by arrays over datatypes or bitvectors. *)
  | SMT.Sort.Set elem_sort ->
    begin match elem_sort with
      | SMT.Sort.Finite _ ->
          let _ = Printf.printf "Translating set as set\n" in
          SMT.Set.mk_enumeration elem_sort []
      | SMT.Sort.Bitvector n ->
          let _ = Printf.printf "Translating set as bitvector\n" in
          let expr' = translate (SMT.Variable.mk (Z3.Expr.to_string) (translate elem_sort)) in

          SMT.Set.mk_enumeration elem_sort []
    end

    (*
  | SMT.Sort.Bitvector n ->
    let name = Z3.Expr.to_string expr in
    let orig_name = SMT.Term.show orig in
    if not @@ String.contains orig_name 'f'
    then
      let _ = Printf.printf "NAME: %s" name in
      SMT.Enumeration.mk_const sort "0"
    else
      let bits = String.sub name 2 (String.length name - 2) in
      Printf.printf "SET %s --> %s: %s\n" orig_name name bits;
      SMT.Set.mk_enumeration sort []
  *)
*)

let aux = function SMT.Variable (name, Set sort) -> SMT.Variable (name, sort)

let eval model term =
  let sort = SMT.Term.get_sort term in
  let value = Option.get @@ Z3.Model.eval model (translate term) false in
  match sort with
  | SMT.Sort.Finite _ -> SMT.Enumeration.mk_const sort (Z3.Expr.to_string value)
  | SMT.Sort.Int ->
      let n = int_of_string @@ Z3.Expr.to_string value in
      SMT.LIA.mk_const n
  (* NOTE: At this point, bitvector should never represent a set. *)
  | SMT.Sort.Bitvector n ->
    Printf.printf "translating bitvector %s...\n" (SMT.Term.show term);
    let bitstring = Z3.Expr.to_string value in
    SMT.Bitvector.mk_const_of_string bitstring
  (* In the model, set can be represented as a bitvector or as a set. *)
  | SMT.Sort.Set elem_sort ->
    (* TODO *)
    Printf.printf "translating set %s ...\n" (SMT.Term.show term);
    begin match elem_sort with
      | SMT.Sort.Finite (_, constants) -> SMT.Set.mk_enumeration sort []
      | SMT.Sort.Bitvector n ->
        let value = Option.get @@ Z3.Model.eval model (translate @@ aux term) false in
        let bits = Z3.Expr.to_string value in
        let set = BitvectorSets.inverse_translation bits n in
        Printf.printf "BV %s ~> %s ~> %s ~> %s\n" (SMT.show term) name bits (SMT.show set);
        set
    end
  | _ -> failwith (
    Format.asprintf "Cannot evaluate SMT expression %s"
      (SMT.show_with_sort term)
  )

(* === Debugging === *)

let show_formula phi = Z3.Expr.to_string phi

let show_model model = Z3.Model.to_string model

let to_smt_benchmark phi =
  Z3.SMT.benchmark_to_smtstring
    !context
    "Input for solver"
    "ALL"
    "unknown"
    ""
    []
    phi
