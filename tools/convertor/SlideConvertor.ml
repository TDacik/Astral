(* Conversion to the input format of the SLIDE solver
 * (see http://www.fit.vutbr.cz/research/groups/verifit/tools/slide).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Convertor_utils

module F = Format

module Convertor = struct
  let name = "Slide"
  let suffix = ".sld"

  let supports_sat = false
  let supports_variadic_operators = false
  let precise_semantics = false
  let supports_ls = true
  let supports_dls = true

  let comment_prefix = "#"

  let set_status context = F.asprintf "# status: %s\n" (Context.show_expected_status context)

  (** Slide does not declare variables *)
  let declare_sort sort = ""
  let declare_var var = ""

  let convert_var var = SSL.Variable.show var

  let declare_ls = "ls(x, y) ::= x = y & emp | \\E l. x->l * ls(l, y)\n\n"

  let declare_dls = "dls(x, y, f, l) ::= x = l & y = f & emp | \\E z. x->z,f * dls(z,y,x,l)\n"

  let rec convert phi =
    if SSL.is_true phi then "true"
    else if SSL.is_false phi then "false"
    else match phi with
    | SSL.Var v -> convert_var v
    | SSL.And (f1, f2) -> F.asprintf "%s & %s" (convert f1) (convert f2)
    | SSL.Star [f1; f2] ->  F.asprintf "%s * %s" (convert f1) (convert f2)
    | SSL.LS (v1, v2) -> F.asprintf "ls(%s, %s)" (convert v1) (convert v2)
    | SSL.DLS (x, y, f, l) ->
        F.asprintf "dll(%s, %s, %s, %s)" (convert x) (convert y) (convert f) (convert l)
    | SSL.PointsTo (v1, [v2]) -> F.asprintf "%s->%s" (convert v1) (convert v2)
    | SSL.PointsTo (x, [p; n]) -> F.asprintf "%s->%s,%s" (convert x) (convert n) (convert p)
    | SSL.Eq [v1; v2] -> F.asprintf "%s = %s" (convert v1) (convert v2)
    | SSL.Distinct [v1; v2] -> F.asprintf "%s != %s" (convert v1) (convert v2)
    | SSL.Exists ([x], psi) -> F.asprintf "\\E %s. %s" (convert x) (convert psi)
    | _ -> raise @@ NotSupported (SSL.node_name phi)

  let convert_aux phi tag =
    let free = SSL.free_vars phi |> List.map SSL.show |> String.concat ", " in
    let header = F.asprintf "%s(%s)" tag free in
    let def = F.asprintf "%s ::= %s" header (convert phi) in
    (header, def)

  let convert_assert (SSL.GuardedNeg (lhs, rhs)) =
    let lhs, lhs_def = convert_aux lhs "LHS" in
    let rhs, rhs_def = convert_aux rhs "RHS" in
    F.asprintf "%s\n\n%s\n\nEntail %s |- %s" lhs_def rhs_def lhs rhs

  let command = ""

end

include Convertor_builder.Make(Convertor)
