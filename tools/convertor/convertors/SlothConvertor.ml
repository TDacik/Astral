(* Conversion to the input format of the Sloth solver (see https://github.com/katelaan/sloth).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open SSL
open SSL.Struct

open Convertor_utils

module F = Format

module Convertor = struct

  let name = "sloth"
  let suffix = ".smt2"

  let supports_sat = true
  let supports_variadic_operators = false
  let precise_semantics = true

  let comment_prefix = ";;"
  let set_status context = Format.asprintf ";; status: %s" (Context.show_expected_status context)

  let global_decls _ = ""

  let declare_sort sort = ""

  let declare_var var =
    if SSL.Variable.is_nil var then ""
    else Format.asprintf "(declare-const %s sl.list.loc)" (SSL.Variable.show var)

  let convert_var var =
    if SSL.Variable.is_nil var then "sl.list.null"
    else SSL.Variable.show var

  let rec convert = function
    | SSL.Emp -> "(sl.list.eq sl.list.null sl.list.null)"
    | SSL.Var v -> convert_var v
    | SSL.And (f1, f2) -> F.asprintf "(and %s %s)\n" (convert f1) (convert f2)
    | SSL.Or (f1, f2) -> F.asprintf "(or %s %s)\n" (convert f1) (convert f2)
    | SSL.Not f ->  F.asprintf "(not %s)\n" (convert f)
    | SSL.GuardedNeg (f1, f2) ->  F.asprintf "(and %s (not %s))\n" (convert f1) (convert f2)
    | SSL.Star [f1; f2] ->  F.asprintf "(sl.sepcon %s %s)\n" (convert f1) (convert f2)
    | SSL.LS (v1, v2) -> F.asprintf "(sl.list.seg %s %s)\n" (convert v1) (convert v2)
    | SSL.PointsTo (x, LS_t n) -> F.asprintf "(sl.list.next %s %s)\n" (convert x) (convert_var n)
    | SSL.Eq [v1; v2] -> F.asprintf "(sl.list.eq %s %s)\n" (convert v1) (convert v2)
    | SSL.Distinct [v1; v2] -> F.asprintf "(sl.list.neq %s %s)\n" (convert v1) (convert v2)
    | other -> raise @@ NotSupported (SSL.node_name other)

  let convert_benchmark phi = F.asprintf "(assert %s)\n(check-sat)" (convert phi)

end

include Convertor_builder.Make(Convertor)
