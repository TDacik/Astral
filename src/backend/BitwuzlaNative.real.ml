(* Native Bitwuzla backend.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

open Backend_sig

module Logger = Logger.MakeWithDir (struct
  let name = "Backend:Bitwuzla-native"
  let level = 1
  let dirname = "unfolding_queries"
end)

module Init () = struct


  module BW = Bitwuzla.Incremental ()

  (* === Declarations === *)

  type formula = [`Bv] BW.term

  type model = unit

  let name = "Bitwuzla-native"

  let supports_smtlib_options = false
  let supports_get_info = false
  let supports_sets = false
  let supports_quantifiers = true

  let is_available () = true

  let init ?timeout () = ()

  (* === Translation === *)

  let translate_bv_sort (Sort.Bitvector n) = BW.Sort.bv n

  let translate_arr_sort (Sort.Array (Sort.Bitvector dom, Bitvector range)) =
    BW.Sort.ar (BW.Sort.bv dom) (BW.Sort.bv range)

  module C = SMT.Variable.Map

  let bv_cache = ref C.empty
  let arr_cache = ref C.empty

  let translate_bv_var var =
    try C.find var !bv_cache
    with Not_found ->
      let name, sort = SMT.Variable.describe var in
      let res = BW.Term.const (translate_bv_sort sort) name in
      bv_cache := C.add var res !bv_cache;
      res

  let translate_arr_var var =
    try C.find var !arr_cache
    with Not_found ->
      let name, sort = SMT.Variable.describe var in
      let res = BW.Term.const (translate_arr_sort sort) name in
      arr_cache := C.add var res !arr_cache;
      res

  let map fn xs = Array.of_list @@ List.map fn xs

  let rec translate_arr t = match SMT.view t with
    | SMT.Variable var -> translate_arr_var var
    | SMT.ConstArr (const, dom_sort) ->
      let sort = Sort.mk_array dom_sort (SMT.get_sort const) in
      BW.Term.Ar.make (translate_arr_sort sort) (translate const)
    | SMT.Store (a, i, v) -> BW.Term.Ar.store (translate_arr a) (translate i) (translate v)

  and translate t = match SMT.view t with
    | SMT.Variable var ->  translate_bv_var var
    | SMT.True -> BW.Term.Bl.true'
    | SMT.False -> BW.Term.Bl.false'

    | SMT.Equal [x; y] -> BW.Term.equal (translate x) (translate y)
    | SMT.Equal (x :: y :: rest) ->
      let step = BW.Term.equal (translate x) (translate y) in
      BW.Term.Bl.logand step (translate @@ SMT.mk_eq (y :: rest))
    | SMT.Distinct ts ->
      List.map translate ts
      |> List_utils.diagonal_product
      |> List.map (fun (t1, t2) -> BW.Term.distinct t1 t2)
      |> List.fold_left BW.Term.Bl.logand BW.Term.Bl.true'

    | SMT.And es -> BW.Term.Bl.redand (map translate es)
    | SMT.Or es -> BW.Term.Bl.redor (map translate es)
    | SMT.Not e -> BW.Term.Bl.lognot (translate e)
    | SMT.Implies (e1, e2) -> BW.Term.Bl.implies (translate e1) (translate e2)
    | SMT.Iff [x; y] -> BW.Term.Bl.iff (translate x) (translate y)
    | SMT.IfThenElse (c, x, y) -> BW.Term.ite (translate c) (translate x) (translate y)

    (* Bitvectors *)
    | SMT.BitConst (number, width) -> BW.Term.Bv.of_int (BW.Sort.bv width) number
    | SMT.BitCheck (bv, index) ->
      let width = BW.Sort.bv @@ SMT.Bitvector.get_width bv in
      let one = BW.Term.Bv.one width in
      let index_expr = translate index in
      let bv_expr = translate bv in
      let mask = BW.Term.Bv.shift_left one index_expr in
      let app = BW.Term.Bv.logand bv_expr mask in
      let zero = BW.Term.Bv.zero width in
      BW.Term.distinct app zero

    | SMT.BitAnd (bvs, Sort.Bitvector n) ->
      let ones = BW.Term.Bv.ones @@ BW.Sort.bv n in
      List.fold_left (fun acc bv -> BW.Term.Bv.logand acc (translate bv)) ones bvs

    | SMT.BitOr (bvs, Sort.Bitvector n) ->
      let zeros = BW.Term.Bv.zero @@ BW.Sort.bv n in
      List.fold_left (fun acc bv -> BW.Term.Bv.logor acc (translate bv)) zeros bvs

    | SMT.BitLesserEqual (e1, e2) -> BW.Term.Bv.ule (translate e1) (translate e2)
    | SMT.BitLesser (e1, e2) -> BW.Term.Bv.ult (translate e1) (translate e2)

    | SMT.BitXor ([bv1; bv2], sort) -> BW.Term.Bv.logxor (translate bv1) (translate bv2)
    | SMT.BitImplies (bv1, bv2) -> BW.Term.Bv.logor (BW.Term.Bv.lognot (translate bv1)) (translate bv2)
    | SMT.BitCompl bv -> BW.Term.Bv.neg (translate bv)
    | SMT.BitShiftLeft (bv, rotate) -> BW.Term.Bv.shift_left (translate bv) (translate rotate)
    | SMT.BitShiftRight (bv, rotate) -> BW.Term.Bv.shift_right_logical (translate bv) (translate rotate)

    | SMT.Select (a, i) -> BW.Term.Ar.select (translate_arr a) (translate i)

    (*
    | SMT.Exists (xs, None, phi) ->
      let binders = List.map translate_var xs in
      Z3.Quantifier.mk_exists_const !context binders (translate phi) None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier

    | SMT.Forall (xs, None, phi) ->
      let binders = List.map translate_var xs in
      Z3.Quantifier.mk_forall_const !context binders (translate phi) None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier
    *)

    | SMT.Forall2 _ | SMT.Exists2 _ ->
      Exceptions.internal_error
        ~reason:"Second order quantifier was not removed before backend translation"
        ~details:(SMT.show t)

    | _ ->
      Exceptions.internal_error
        ~reason:"[Bitwuzla wrapper] unknown term"
        ~details:(SMT.show t)

  let translate_model _ _ _ = ()

  (* ==== Solver ==== *)

  let solve context phi_orig produce_models options =
    match BW.check_sat_assuming [| translate phi_orig |] with
      | Sat -> SMT_Sat None
      | Unsat -> SMT_Unsat []
      | Unknown -> SMT_Unknown ""

  let simplify phi = phi

  (* === Incremental solving === *)

  let cnt = ref 0

  let next () = incr cnt; Format.asprintf "query%04d.smt2" !cnt

  let push phi =
    BW.push 1;
    BW.assert' (translate phi)

  let pop n = BW.pop n

  let check_sat phi =
    push phi;
    (* Timeout as parameter *)
    let res = match BW.timeout 1. BW.check_sat () with
      | Sat -> SMT_Sat None
      | Unsat -> SMT_Unsat []
      | Unknown -> SMT_Unknown ""
    in
    pop 1;
    res

  (* === Debugging === *)

  let show_formula phi = Format.asprintf "%a" BW.Term.pp phi

  let show_model model = ""

  let to_smtlib phi _ _ = "--"

end
