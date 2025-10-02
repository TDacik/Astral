(* Native cvc5 backend
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025

open Backend_sig
open Encoding_context_sig

module Logger = Logger.MakeWithDir (struct
  let name = "Backend:cvc5"
  let level = 1
  let dirname = "unfolding_queries"
end)

(** Generative module prevents initialization of cvc5 when it is not used *)
module Init ( ) = struct

  (* === Declarations === *)

  type formula = unit

  type model = unit

  let name = "cvc5"

  let supports_smtlib_options = true
  let supports_get_info = true
  let supports_sets = true
  let supports_quantifiers = true

  let is_available () = true

  let init ?timeout () = failwith "Not implemented"

  let translate term = match SMT.view term with
    | SMT.Variable var -> translate_var var
    | SMT.True -> Term.mk_true
    | SMT.False -> Term.mk_false

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


  let solve _ _ _ _ = failwith "Not implemented"
  let simplify _ = failwith "Not implemented"
  let show_formula _ = failwith "Not implemented"
  let to_smtlib _ _ _ = failwith "Not implemented"
  let show_model _ = failwith "Not implemented"
  let push _ = failwith "Not implemented"
  let pop _ = failwith "Not implemented"
  let check_sat _ = failwith "Not implemented"

end*)
