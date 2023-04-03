(* Conversion to the input format of the cvc5 solver
 * (see https://cvc5.github.io/docs/cvc5-1.0.2/theories/separation-logic.html).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Convertor_utils

module F = Format

module Convertor = struct
  let name = "cvc5"
  let suffix = ".smt2"

  let supports_sat = true
  let supports_variadic_operators = true
  let precise_semantics = false
  let supports_ls = false
  let supports_dls = false

  let comment_prefix = ";;"

  let set_status context =
    F.asprintf "(set-info :status %s)" (Context.show_expected_status context)

  let declare_sort sort =
    Format.asprintf "(declare-sort %s 0)" (Sort.show sort)

  let declare_var var =
    if SSL.Variable.is_nil var then ""
    else Format.asprintf "(declare-const %s Loc)" (SSL.Variable.show var)

  let convert_var var =
    if SSL.Variable.is_nil var then "sep.nil"
    else SSL.Variable.show var

  let declare_ls = ""
  let declare_dls = ""

  let rec convert = function
    | SSL.Var v -> convert_var v
    | SSL.Pure term -> failwith "TODO"
    | SSL.And (f1, f2) -> F.asprintf "(and %s %s)\n" (convert f1) (convert f2)
    | SSL.Or (f1, f2) -> F.asprintf "(or %s %s)\n" (convert f1) (convert f2)
    | SSL.Not f ->  F.asprintf "(not %s)\n" (convert f)
    | SSL.GuardedNeg (f1, f2) ->  F.asprintf "(and %s (not %s))\n" (convert f1) (convert f2)
    | SSL.Star [f1; f2] ->  F.asprintf "(sep %s %s)\n" (convert f1) (convert f2)
    | SSL.PointsTo (v1, [v2]) -> F.asprintf "(pto %s %s)\n" (convert v1) (convert v2)
    | SSL.Eq [v1; v2] -> F.asprintf "(= %s %s)\n" (convert v1) (convert v2)
    | SSL.Distinct [v1; v2] -> F.asprintf "(distinct %s %s)\n" (convert v1) (convert v2)
    | other -> raise @@ NotSupported (SSL.node_name other)

  let convert_assert phi = F.asprintf "(assert %s)" (convert phi)

  let command = "(check-sat)"

end

include Convertor_builder.Make(Convertor)
