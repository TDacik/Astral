(* Conversion to the input format of the GRASShopper tool (https://github.com/wies/grasshopper).
 *
 * Since GRAShopper is a verification tool rather than solver, the input formula is translated
 * to an equivalent verification problem of an empty program.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Convertor_utils

module F = Format

module Convertor = struct
  let name = "GRASShopper"
  let suffix = ".spl"

  let supports_sat = false
  let supports_variadic_operators = false
  let precise_semantics = true
  let supports_ls = true
  let supports_dls = false (* TODO: check *)

  let comment_prefix = "//"

  let set_status context = Format.asprintf "// status: %s" (Context.show_expected_status context)

  let declare_var var =
    if SSL.Variable.is_nil var then ""
    else Format.asprintf "(%s : Node)" (SSL.Variable.show var)

  let convert_var var =
    if SSL.Variable.is_nil var then "null"
    else SSL.Variable.show var

  let declare_ls = " \
struct Node { \
  var next: Node; \
} \
\
predicate lseg(x: Node, y: Node) {\
  acc({ z: Node :: Btwn(next, x, z, y) && z != y }) &*& Reach(next, x, y)\
}\
"

  let declare_dls = ""

  let rec convert = function
    | SSL.Var v -> convert_var v
    | SSL.Pure term -> failwith "TODO"
    | SSL.And (f1, f2) -> F.asprintf "(%s && %s)" (convert f1) (convert f2)
    | SSL.Or (f1, f2) -> F.asprintf "(%s || %s)" (convert f1) (convert f2)
    | SSL.Not f ->  F.asprintf "(!%s)\n" (convert f)
    | SSL.Star [f1; f2] ->  F.asprintf "(%s &*& %s)" (convert f1) (convert f2)
    | SSL.LS (v1, v2) -> F.asprintf "(lseg (%s, %s))" (convert v1) (convert v2)
    | SSL.PointsTo (v1, [v2]) -> F.asprintf "(%s.next |-> %s)" (convert v1) (convert v2)
    | SSL.Eq [v1; v2] -> F.asprintf "(%s == %s)" (convert v1) (convert v2)
    | SSL.Distinct [v1; v2] -> F.asprintf "(%s != %s)" (convert v1) (convert v2)
    | SSL.GuardedNeg (f1, f2) -> F.asprintf "(%s && (!%s))" (convert f1) (convert f2)
    | other -> raise @@ NotSupported (SSL.node_name other)

  let convert_assert phi = "TODO"

  let command = "TODO"

end

include Convertor_builder.Make(Convertor)
