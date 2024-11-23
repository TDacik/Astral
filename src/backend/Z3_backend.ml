(* Z3 adapter for Astral
 *
 * TODO: pass user defined options
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Backend_sig
open Encoding_context_sig

open Z3enums

module Logger = Logger.Make(struct let name = "Backend:Z3" let level = 3 end)

(** Generative module prevenets initialization of Z3 when it is not used *)
module Init () = struct

  (* === Declarations === *)

  type formula = Z3.Expr.expr

  type model = Z3.Model.model

  let name = "Z3"

  let supports_smtlib_options = true
  let supports_get_info = true
  let supports_sets = true
  let supports_quantifiers = true

  (* === Initialization === *)

  let context = ref (Z3.mk_context [])

  let solver = ref (Z3.Solver.mk_simple_solver !context)

  (** Enumeration sort in Z3 cannot be declared multiple times. We thus
      use a simple cache for a location sort. This works as long as there
      is a SINGLE enumeration sort. *)
  let enum_sort = ref (None : Z3.Sort.sort option)

  let is_available () = true

  let init () =
    context := Z3.mk_context [];
    solver := Z3.Solver.mk_simple_solver !context

  (* === Translation === *)

  let rec translate_var var =
    Z3.Expr.mk_const_s !context (SMT.Variable.show var) (translate_sort @@ SMT.Variable.get_sort var)

  and translate t = match SMT.view t with
    | SMT.Constant (name, sort) -> find_const name sort
    | SMT.Variable var ->  translate_var var

    | SMT.True -> Z3.Boolean.mk_true !context
    | SMT.False -> Z3.Boolean.mk_false !context
    | SMT.Equal [x; y] -> Z3.Boolean.mk_eq !context (translate x) (translate y)
    | SMT.Equal (x :: y :: rest) ->
      let step = Z3.Boolean.mk_eq !context (translate x) (translate y) in
      Z3.Boolean.mk_and !context [step; translate @@ SMT.mk_eq @@ y :: rest]
    | SMT.Distinct ts -> Z3.Boolean.mk_distinct !context (List.map translate ts)
    | SMT.And es -> Z3.Boolean.mk_and !context (List.map translate es)
    | SMT.Or es -> Z3.Boolean.mk_or !context (List.map translate es)
    | SMT.Not e -> Z3.Boolean.mk_not !context (translate e)
    | SMT.Implies (e1, e2) -> Z3.Boolean.mk_implies !context (translate e1) (translate e2)
    | SMT.Iff [x] -> translate x
    | SMT.Iff (x :: y :: rest) -> (* TODO: also check *)
      let step = Z3.Boolean.mk_eq !context (translate x) (translate y) in
      Z3.Boolean.mk_and !context [step; translate @@ SMT.mk_eq @@ y :: rest]
    | SMT.IfThenElse (c, x, y) ->
      Z3.Boolean.mk_ite !context (translate c) (translate x) (translate y)

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

    | SMT.BitAnd (bvs, Sort.Bitvector n) ->
      let ones = Z3.BitVector.mk_repeat !context n (Z3.BitVector.mk_numeral !context "1" 1) in
      List.fold_left (fun acc bv -> Z3.BitVector.mk_and !context acc (translate bv)) ones bvs

    | SMT.BitOr (bvs, Sort.Bitvector n) ->
      let zeros = Z3.BitVector.mk_numeral !context "0" n in
      List.fold_left (fun acc bv -> Z3.BitVector.mk_or !context acc (translate bv)) zeros bvs

    | SMT.BitLesserEqual (e1, e2) -> Z3.BitVector.mk_ule !context (translate e1) (translate e2)
    | SMT.BitLesser (e1, e2) -> Z3.BitVector.mk_ult !context (translate e1) (translate e2)


    | SMT.BitXor ([bv1; bv2], sort) ->
      Z3.BitVector.mk_xor !context (translate bv1) (translate bv2)
    | SMT.BitImplies (bv1, bv2) ->
      Z3.BitVector.mk_or !context (Z3.BitVector.mk_not !context (translate bv1)) (translate bv2)
    | SMT.BitCompl bv -> Z3.BitVector.mk_not !context (translate bv)
    | SMT.BitShiftLeft (bv, rotate) ->
      Z3.BitVector.mk_shl !context (translate bv) (translate rotate)
    | SMT.BitShiftRight (bv, rotate) ->
      Z3.BitVector.mk_lshr !context (translate bv) (translate rotate)

    | SMT.Disjoint [s1; s2] ->
      let intersect = Z3.Set.mk_intersection !context [translate s1; translate s2] in
      let sort = translate_sort @@ SMT.Sets.get_elem_sort s1 in
      let empty = Z3.Set.mk_empty !context sort in
      Z3.Boolean.mk_eq !context empty intersect

    | SMT.Enumeration (elements, sort) ->
      let empty = Z3.Set.mk_empty !context (translate_sort (Sort.get_dom_sort sort)) in
      List.map translate elements
      |> List.fold_left (Z3.Set.mk_set_add !context) empty

    | SMT.ConstArr (const, _) ->
      Z3.Z3Array.mk_const_array !context (Z3.Expr.get_sort @@ translate const) (translate const)
    | SMT.Select (a, i) ->
      Z3.Z3Array.mk_select !context (translate a) (translate i)
    | SMT.Store (a, i, v) ->
      Z3.Z3Array.mk_store !context (translate a) (translate i) (translate v)

    | SMT.IntConst i -> Z3.Arithmetic.Integer.mk_numeral_i !context i
    | SMT.Plus es -> Z3.Arithmetic.mk_add !context @@ List.map translate es
    | SMT.Minus (e1, e2) -> Z3.Arithmetic.mk_sub !context [translate e1; translate e2]
    | SMT.Mult es -> Z3.Arithmetic.mk_mul !context @@ List.map translate es

    | SMT.Exists (xs, None, phi) ->
      let binders = List.map translate_var xs in
      Z3.Quantifier.mk_exists_const !context binders (translate phi) None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier

    | SMT.Forall (xs, None, phi) ->
      let binders = List.map translate_var xs in
      Z3.Quantifier.mk_forall_const !context binders (translate phi) None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier

    | SMT.Forall2 _ | SMT.Exists2 _ ->
      Utils.internal_error
        "second order quantification should be removed before translating to backend solver"

    | _ -> failwith (Format.asprintf "Z3 wrapper error at: %s" (SMT.show t))

  and translate_sort = function
    | Sort.Bool -> Z3.Boolean.mk_sort !context
    | Sort.Int -> Z3.Arithmetic.Integer.mk_sort !context
    | Sort.Bitvector n -> Z3.BitVector.mk_sort !context n
    | Sort.Set (elem_sort) -> Z3.Set.mk_sort !context (translate_sort elem_sort)
    | Sort.Array (d, r) -> Z3.Z3Array.mk_sort !context (translate_sort d) (translate_sort r)
    | Sort.Finite (name, constants) ->
      begin match !enum_sort with
        | Some sort -> sort
        | None ->
          let name = Identifier.show name in
          let sort = Z3.Enumeration.mk_sort_s !context name constants in
          enum_sort := Some sort;
          sort
      end
    | sort -> Utils.internal_error ("Cannot translate sort " ^ Sort.show sort)

  and find_const const sort =
    let sort = translate_sort sort in
    let consts = Z3.Enumeration.get_consts sort in
    List.find (fun c -> String.equal (Z3.Expr.to_string c) const) consts

  (* ==== Model translation ==== *)

  (* TODO: this is very slow for bitvectors. In fact, we need to check only first n values... *)
  let get_universe sort = match sort with
    | Sort.Finite _ -> SMT.Enumeration.get_constants sort
    | Sort.Bitvector width ->
      BatList.range 0 `To (BatInt.pow 2 width)
      |> List.map (fun i -> Constant.mk_bitvector_of_int i width)

  let translate_constant astral_sort z3_expr =
    let sort = Z3.Expr.get_sort z3_expr in
    match Z3.Sort.get_sort_kind sort with
    | BOOL_SORT -> Constant.mk_bool @@ Z3.Boolean.is_true z3_expr
    | INT_SORT -> Constant.mk_int @@ int_of_string @@ Z3.Expr.to_string z3_expr
    | BV_SORT -> Constant.mk_bitvector_of_string @@ Z3.Expr.to_string z3_expr
    | DATATYPE_SORT -> Constant.mk_const astral_sort @@ Z3.Expr.to_string z3_expr
    | _ -> failwith @@ Format.asprintf "Unknown Z3 sort: %s\n" (Z3.Sort.to_string sort)

  let rec translate_m astral_ctx z3_model z3_array =
    try
      let operands = Z3.Expr.get_args z3_array in
      let operands' = List.map (translate_m astral_ctx z3_model) operands in
      match (Z3.FuncDecl.get_decl_kind @@ Z3.Expr.get_func_decl z3_array), operands' with
      | OP_TRUE, _ -> Constant.tt
      | OP_FALSE, _ -> Constant.ff

      | OP_CONST_ARRAY, [default] -> Constant.mk_array ~default []
      | OP_STORE, [arr; index; value] -> Constant.array_add_binding arr index value

      | _, _ -> translate_constant astral_ctx.loc_sort z3_array

    with Z3.Error _ ->
      let str = Z3.Expr.to_string z3_array in
      if String.equal str "(lambda ((x!1 Locations)) x!1)" then Constant.Array Constant.Identity
      else failwith str


  let translate_model_var astral_context z3_model var =
    let term = SMT.of_var var in
    let sort = SMT.get_sort term in
    let interp = Option.get @@ Z3.Model.eval z3_model (translate term) false in
    Logger.debug "Translating variale %s = %s\n"
      (SMT.Variable.show_with_sort var) (Z3.Expr.to_string interp);
    match sort with
    | Sort.Bool | Sort.Int | Sort.Bitvector _ | Sort.Finite _ -> translate_constant sort interp

    | Sort.Set dom ->
      let consts = get_universe dom in
      BatList.filter
        (fun c ->
          let z3_query = Z3.Set.mk_membership !context (translate @@ SMT.of_const c) (translate term) in
          let res = Option.get @@ Z3.Model.eval z3_model z3_query false in
          Z3.Boolean.is_true res
        ) consts
      |> Constant.mk_set


    | Sort.Array (dom, range) ->
      let arr = SMT.of_var var in
      let res = Z3.Model.get_const_interp_e z3_model (translate arr) in
      let res = BatOption.get_exn res (Failure ("No interpretation for array " ^ SMT.show arr)) in
      translate_m astral_context z3_model res

  (** Translate Z3's model. This function assumes that all uninterpreted sorts are finite. *)
  let translate_model context phi z3_model =
    let vars = SMT.free_vars phi in
    List.fold_left
      (fun acc var ->
        let term = translate_model_var context z3_model var in
        SMT.Model.add var term acc
      ) SMT.Model.empty vars

  (* ==== Solver ==== *)

  let solve context phi_orig produce_models options =
    let phi = translate phi_orig in
    match Z3.Solver.check !solver [phi] with
    | Z3.Solver.SATISFIABLE ->
      if produce_models then
        let model = Option.get @@ Z3.Solver.get_model !solver in
        let _ = Debug.backend_model @@ Z3.Model.to_string model in
        SMT_Sat (Some (translate_model context phi_orig model, model))
      else
        SMT_Sat None

    (* TODO: unsat core *)
    | Z3.Solver.UNSATISFIABLE ->
      let unsat_core = [] in
      SMT_Unsat unsat_core

    | Z3.Solver.UNKNOWN ->
      let reason = Z3.Solver.get_reason_unknown !solver in
      SMT_Unknown reason

  let simplify phi = Z3.Expr.simplify phi None

  (* === Debugging === *)

  let show_formula phi = Z3.Expr.to_string phi

  let show_model model = Z3.Model.to_string model

  (* TODO: models and options *)
  let to_smtlib phi _ _ =
    Z3.SMT.benchmark_to_smtstring
      !context
      "Input for solver"
      "ALL"
      "unknown"
      ""
      []
      (translate phi)

end
