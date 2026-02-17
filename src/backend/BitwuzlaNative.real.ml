(* Native Bitwuzla backend.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

open Backend_sig

module Logger = Debug.QueryDir (struct
  let name = "Backend:Bitwuzla-native"
  let level = 1
  let dirname = "unfolding_queries"
end)

module Bitwuzla = Bitwuzla_cxx
module Options = Bitwuzla.Options

let is_available () = true

module Init () = struct


  module BW = Bitwuzla.Make()

  (* === Declarations === *)

  type formula = BW.Term.t

  type model = unit

  let name = "Bitwuzla-native"

  let supports_smtlib_options = false
  let supports_get_info = false
  let supports_sets = false
  let supports_quantifiers = true

  let is_available = is_available

  let solver = ref @@ BW.Solver.create @@ Options.default ()

  let timeout limit =
    let start = Unix.gettimeofday () in
    fun () ->
      let now = Unix.gettimeofday () in
      Float.compare (now -. start) limit >= 0

  (* TODO: produce models only on demand *)
  let init ?timeout () =
    let options = Options.default () in
    Options.set options Options.Produce_models true;
    solver := BW.Solver.create options

  (* === Translation === *)

  let rec translate_sort = function
    | Sort.Bitvector n -> BW.mk_bv_sort n
    | Sort.Array (dom, range) -> BW.mk_array_sort (translate_sort dom) (translate_sort range)

  module C = SMT.Variable.Map

  let bv_cache = ref C.empty

  let translate_var var =
    try C.find var !bv_cache
    with Not_found ->
      let symbol, sort = SMT.Variable.describe var in
      let res = BW.mk_var ~symbol (translate_sort sort) in
      bv_cache := C.add var res !bv_cache;
      res

  let translate_const var =
    try C.find var !bv_cache
    with Not_found ->
      let symbol, sort = SMT.Variable.describe var in
      let res = BW.mk_const ~symbol (translate_sort sort) in
      bv_cache := C.add var res !bv_cache;
      res

  let map fn xs = Array.of_list @@ List.map fn xs

  let rec translate t =
    match SMT.view t with
    | SMT.Variable var -> translate_const var
    | SMT.True -> BW.mk_true ()
    | SMT.False -> BW.mk_false ()

    | SMT.Equal [x; y] -> BW.mk_term2 Equal (translate x) (translate y)
    | SMT.Equal (x :: y :: rest) ->
      let step = BW.mk_term2 Equal (translate x) (translate y) in
      BW.mk_term2 And step (translate @@ SMT.mk_eq (y :: rest))
    | SMT.Distinct ts ->
      List.map translate ts
      |> List_utils.diagonal_product
      |> List.map (fun (t1, t2) -> BW.mk_term2 Distinct t1 t2)
      |> List.fold_left (BW.mk_term2 And) (BW.mk_true ())

    | SMT.And [x; y] -> BW.mk_term2 And (translate x) (translate y)
    | SMT.And (x :: rest) -> BW.mk_term2 And (translate x) (translate @@ SMT.Boolean.mk_and rest)

    | SMT.Or [x; y] -> BW.mk_term2 Or (translate x) (translate y)
    | SMT.Or (x :: rest) -> BW.mk_term2 Or (translate x) (translate @@ SMT.Boolean.mk_or rest)

    | SMT.Not x -> BW.mk_term1 Not (translate x)

    | SMT.Implies (e1, e2) -> BW.mk_term2 Implies (translate e1) (translate e2)
    | SMT.Iff [x; y] -> BW.mk_term2 Iff  (translate x) (translate y)
    | SMT.IfThenElse (c, x, y) -> BW.mk_term3 Ite (translate c) (translate x) (translate y)

    (* Bitvectors *)
    | SMT.BitConst (number, width) -> BW.mk_bv_value_int (BW.mk_bv_sort width) number
    | SMT.BitCheck (bv, index) ->
      (* TODO: use extract? *)
      let width = BW.mk_bv_sort @@ SMT.Bitvector.get_width bv in
      let one = BW.mk_bv_one width in
      let index_expr = translate index in
      let bv_expr = translate bv in
      let mask = BW.mk_term2 Bv_shl one index_expr in
      let app = BW.mk_term2 Bv_and bv_expr mask in
      let zero = BW.mk_bv_zero width in
      BW.mk_term2 Distinct app zero

    | SMT.BitAnd (bvs, Sort.Bitvector n) ->
      let ones = BW.mk_bv_ones @@ BW.mk_bv_sort n in
      List.fold_left (fun acc bv -> BW.mk_term2 Bv_and acc (translate bv)) ones bvs

    | SMT.BitOr (bvs, Sort.Bitvector n) ->
      let zeros = BW.mk_bv_zero @@ BW.mk_bv_sort n in
      List.fold_left (fun acc bv -> BW.mk_term2 Bv_or acc (translate bv)) zeros bvs

    | SMT.BitLesserEqual (e1, e2) -> BW.mk_term2 Bv_ule (translate e1) (translate e2)
    | SMT.BitLesser (e1, e2) -> BW.mk_term2 Bv_ult (translate e1) (translate e2)

    | SMT.BitXor ([bv1; bv2], sort) -> BW.mk_term2 Bv_xor (translate bv1) (translate bv2)
    | SMT.BitImplies (bv1, bv2) -> BW.mk_term2 Bv_or (BW.mk_term1 Bv_not (translate bv1)) (translate bv2)
    | SMT.BitCompl bv -> BW.mk_term1 Bv_not (translate bv)
    | SMT.BitShiftLeft (bv, rotate) -> BW.mk_term2 Bv_shl (translate bv) (translate rotate)
    | SMT.BitShiftRight (bv, rotate) -> BW.mk_term2 Bv_shr (translate bv) (translate rotate)

    | SMT.BitPlus ([x; y], _) -> BW.mk_term2 Bv_add (translate x) (translate y)
    | SMT.BitPlus (x :: xs, width) -> BW.mk_term2 Bv_add (translate x) (translate @@ SMT.Bitvector.mk_plus (Sort.get_width width) xs)

    | SMT.Select (a, i) -> BW.mk_term2 Select (translate a) (translate i)
    | SMT.ConstArr (const, dom_sort) ->
      let sort = Sort.mk_array dom_sort (SMT.get_sort const) in
      BW.mk_term1 Const_array (translate const)
    | SMT.Store (a, i, v) -> BW.mk_term3 Store (translate a) (translate i) (translate v)

    | SMT.Exists (xs, None, phi) ->
      let x :: xs = List.map translate_var xs in
      let acc0 = BW.mk_term2 Exists x (translate phi) in
      List.fold_left (fun acc y -> BW.mk_term2 Exists y acc) acc0 xs

    | SMT.Forall (xs, None, phi) ->
      let x :: xs = List.map translate_var xs in
      let acc0 = BW.mk_term2 Forall x (translate phi) in
      List.fold_left (fun acc y -> BW.mk_term2 Forall y acc) acc0 xs

    | SMT.Forall2 _ | SMT.Exists2 _ ->
      Exceptions.internal_error
        ~reason:"Second order quantifier was not removed before backend translation"
        ~details:(SMT.show t)

    | _ ->
      Exceptions.internal_error
        ~reason:"[Bitwuzla wrapper] unknown term"
        ~details:(SMT.show t)

  (* ==== Model translation ==== *)

  let bitvector_to_const (bv : BW.Term.t) =
    Constant.mk_bitvector_of_string @@ BW.Term.to_string bv

  let rec array_to_const (arr : BW.Term.t) =
    match BW.Term.kind (arr : BW.Term.t) with
    | Const_array ->
      let default = bitvector_to_const @@ BW.Term.get arr 0 in
      Constant.mk_array ~default []
    | Store ->
      let arr' = array_to_const @@ BW.Term.get arr 0 in
      let index = bitvector_to_const @@ BW.Term.get arr 1 in
      let value = bitvector_to_const @@ BW.Term.get arr 2 in
      Constant.array_add_binding arr' index value

  let translate_model solver phi =
    (* Bitwuzla does not provide model explicitly *)
    let vars = SMT.free_vars phi in
    List.fold_left (fun acc var ->
      let value = BW.Solver.get_value solver @@ translate_var var in
      let c = match SMT.Variable.get_sort var with
        | Bitvector _ -> bitvector_to_const value
        | Array _ -> array_to_const value
      in
      SMT.Model.add var c acc
    ) SMT.Model.empty vars

  (* ==== Solver ==== *)

  let solve context phi_orig produce_models options =
    let options = Options.default () in
    Options.set options Options.Produce_models true;
    let solver = BW.Solver.create options in
    match BW.Solver.check_sat ~assumptions:[|translate phi_orig|] solver with
      | Sat -> SMT_Sat (Option.some (translate_model solver phi_orig, ())) (* TODO: do this on-demand *)
      | Unsat -> SMT_Unsat []
      | Unknown -> SMT_Unknown ""

  let simplify phi = phi

  (* === Incremental solving === *)

  let cnt = ref 0

  let next () = incr cnt; Format.asprintf "query%04d.smt2" !cnt

  let push phi =
    BW.Solver.push !solver 1;
    BW.Solver.assert_formula !solver (translate phi)

  let pop n = BW.Solver.pop !solver n

  let check_sat phi =
    push phi;
    (* Timeout as parameter *)
    BW.Solver.configure_terminator !solver (Option.some @@ timeout 1.0);
    let res = match BW.Solver.check_sat !solver with
      | Sat -> SMT_Sat None
      | Unsat -> SMT_Unsat []
      | Unknown -> SMT_Unknown ""
    in
    pop 1;
    res

  (* === Debugging === *)

  let show_formula phi = "" (*Format.asprintf "%a" BW.Term.pp phi*)

  let show_model model = ""

  let to_smtlib phi _ _ = "--"

end

let () = BackendConfig.register_native "bitwuzla" ~package:"bitwuzla-cxx" ~available:true
