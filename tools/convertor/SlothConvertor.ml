(* Conversion to the input format of the Sloth solver (see https://github.com/katelaan/sloth).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Convertor_utils

module F = Format

module Convertor = struct
  let name = "sloth"
  let suffix = ".smt2"

  let supports_sat = true
  let supports_variadic_operators = false
  let precise_semantics = true
  let supports_ls = true
  let supports_dls = false

  let comment_prefix = ";;"

  let set_status context = Format.asprintf ";; status: %s" (Context.show_expected_status context)

  let declare_var var =
    if SSL.Variable.is_nil var then ""
    else Format.asprintf "(declare-const %s sl.list.loc)" (SSL.Variable.show var)

  let convert_var var =
    if SSL.Variable.is_nil var then "sl.list.null"
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
    | SSL.Star [f1; f2] ->  F.asprintf "(sl.sepcon %s %s)\n" (convert f1) (convert f2)
    | SSL.LS (v1, v2) -> F.asprintf "(sl.list.seg %s %s)\n" (convert v1) (convert v2)
    | SSL.PointsTo (v1, [v2]) -> F.asprintf "(sl.list.next %s %s)\n" (convert v1) (convert v2)
    | SSL.Eq [v1; v2] -> F.asprintf "(sl.list.eq %s %s)\n" (convert v1) (convert v2)
    | SSL.Distinct [v1; v2] -> F.asprintf "(sl.list.neq %s %s)\n" (convert v1) (convert v2)
    | other -> raise @@ NotSupported (SSL.node_name other)

  let convert_assert phi = F.asprintf "(assert %s)" (convert phi)

  let command = "(check-sat)"

end

include Convertor_builder.Make(Convertor)
