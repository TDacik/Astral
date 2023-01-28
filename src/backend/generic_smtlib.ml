(* Generic backend for SMT-LIB compliant solver runned from command-line
 *
 * This backend can be further extended to support non-standard extension of solver
 * (see CVC5_backend.ml).
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2023 *)

(** ==== Translation ==== *)

exception NonStandardTerm

let rec translate_decl (SMT.Variable (name, sort)) translate_sort =
  Format.asprintf "(declare-const %s %s)" name (translate_sort sort)

(** Translation of standard terms. Translation of non-standard term raises exception
    that should be handled by concrete solver. *)
and translate_std translate translate_sort term = match term with
  | SMT.Variable (name, _) -> name
  | SMT.Constant (name, _) -> name
  | SMT.True -> "true"
  | SMT.False -> "false"
  | SMT.Equal (t1, t2) -> Format.asprintf "(= %s %s)" (translate t1) (translate t2)
  | SMT.Distinct ts -> Format.asprintf "(distinct %s)" (translate_expr_list translate ts)

  | SMT.And ts -> Format.asprintf "(and %s)" (translate_expr_list translate ts)
  | SMT.Or ts -> Format.asprintf "(or %s)" (translate_expr_list translate ts)
  | SMT.Not t -> Format.asprintf "(not %s)" (translate t)
  | SMT.Implies (t1, t2) -> Format.asprintf "(=> %s %s)" (translate t1) (translate t2)
  | SMT.Iff (t1, t2) -> Format.asprintf "(= %s %s)" (translate t1) (translate t2)

  | SMT.LesserEq (t1, t2) -> begin match SMT.Term.get_sort t1 with
    | Sort.Bitvector _ -> Format.asprintf "(bvule %s %s)" (translate t1) (translate t2)
    (* TODO: other sorts *)
  end

  | SMT.ConstArr _ -> failwith "not implemented"
  | SMT.Select (a, i) -> Format.asprintf "(select %s %s)" (translate a) (translate i)
  | SMT.Store (a, i, x) ->
    Format.asprintf "(store %s %s %s)" (translate a) (translate i) (translate x)

  (* Bitvectors *)
  | SMT.BitConst (number, width) ->
    Format.asprintf "(_ bv%d %d)" number width
  | SMT.BitCheck (bv, index) ->
    let width = SMT.Bitvector.get_width bv in
    Format.asprintf "(distinct (bvand (bvshl (_ bv1 %d) %s) %s) (_ bv0 %d))"
      width
      (translate index)
      (translate bv)
      width

  | SMT.BitAnd ([bv1; bv2], sort) ->
    Format.asprintf "(bvand %s %s)" (translate bv1) (translate bv2)
  | SMT.BitOr (bvs, Sort.Bitvector n) ->
    let zeros = Format.asprintf "(_ bv0 %d)" n in
    List.fold_left (fun acc bv -> Format.asprintf "(bvor %s %s)" acc (translate bv)) zeros bvs

  | SMT.BitXor ([bv1; bv2], sort) ->
    Format.asprintf "(bvxor %s %s)" (translate bv1) (translate bv2)
  | SMT.BitImplies (bv1, bv2) ->
    Format.asprintf "(bvor (bvnot %s) %s)" (translate bv1) (translate bv2)
  | SMT.BitCompl bv ->
    Format.asprintf "(bvnot %s)" (translate bv)
  | SMT.BitShiftLeft (bv, rotate) ->
    Format.asprintf "(bvshl %s %s)" (translate bv) (translate rotate)
  | SMT.BitShiftRight (bv, rotate) ->
    Format.asprintf "(bvlshr %s %s)" (translate bv) (translate rotate)

  (* TODO: instantiation should be done somewhere else *)
  | SMT.Forall (x :: _, phi) ->
    let x_sort = SMT.Term.get_sort x in
    begin match x_sort with
    | Finite (_, cs) ->
      let es = List.map (fun c -> SMT.Term.substitute phi x (SMT.Constant (c, x_sort))) cs in
      Format.asprintf "(and %s)" (translate_expr_list translate es)
    | _ ->
      Format.asprintf "(forall ((%s %s)) %s)"
        (translate x)
        (translate_sort x_sort)
        (translate phi)
    end

  | SMT.Exists (x :: _, phi) ->
    let x_sort = SMT.Term.get_sort x in
    begin match x_sort with
    | Finite (_, cs) ->
      let es = List.map (fun c -> SMT.Term.substitute phi x (SMT.Constant (c, x_sort))) cs in
      Format.asprintf "(or %s)" (translate_expr_list translate es)
    | _ ->
      Format.asprintf "(exists ((%s %s)) %s)"
        (translate x)
        (translate_sort x_sort)
        (translate phi)
    end

  | SMT.IntConst i -> Format.asprintf "%d" i
  | SMT.Plus (t1, t2) -> Format.asprintf "(+ %s %s)" (translate t1) (translate t2)
  | SMT.Minus (t1, t2) -> Format.asprintf "(- %s %s)" (translate t1) (translate t2)
  | SMT.Mult (t1, t2) -> Format.asprintf "(* %s %s)" (translate t1) (translate t2)

  | SMT.Forall2 _ | SMT.Exists2 _ ->
    failwith "Internal error: second order quantification should be removed before \
              translating to backend solver"

  (* Raise exception for non-standard terms *)
  | _ -> raise NonStandardTerm

and translate_std_sort translate_sort = function
  | Sort.Bool -> "Bool"
  | Sort.Int -> "Int"
  | Sort.Bitvector n -> Format.asprintf "(_ BitVec %d)" n
  | Sort.Array (d, r) -> "(Array " ^ (translate_sort d) ^ " " ^ (translate_sort r) ^ ")"

  (* Raise exception for non-standard sorts *)
  | _ -> raise NonStandardTerm

and translate_expr_list translate exprs =
  List.map translate exprs
  |> String.concat " "

(** All currently used standard sort do not need any declaration. *)
let declare_std_sort _ = ""
