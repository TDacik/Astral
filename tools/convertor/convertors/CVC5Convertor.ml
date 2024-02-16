(* Conversion to the input format of the cvc5 solver
 * (see https://cvc5.github.io/docs/cvc5-1.0.2/theories/separation-logic.html).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Struct

open Context
open Convertor_utils

module F = Format

module Convertor = struct
  let name = "cvc5"
  let suffix = ".smt2"

  let supports_sat = true
  let supports_variadic_operators = true
  let precise_semantics = false

  let global_decls context =
    F.asprintf "(declare-heap %s)" @@ ParserContext.show_heap_sort context.raw_input

  let declare_var var =
    if SSL.Variable.is_nil var then ""
    else Format.asprintf "(declare-const %s Loc)" (SSL.Variable.show var)

  let declare_sort sort =
    Format.asprintf "(declare-sort %s 0)" (Sort.show sort)

  let comment_prefix = ";;"

  let set_status context =
    F.asprintf "(set-info :status %s)" (Context.show_expected_status context)

  let convert_var var =
    if SSL.Variable.is_nil var then "(as sep.nil Loc)"
    else SSL.Variable.show var

  let convert_binders xs =
    List.map
      (fun x -> match x with
        | SSL.Var (name, sort) -> F.asprintf "(%s %a)" name Sort.pp sort
        | _ -> assert false
      ) xs
    |> String.concat " "

  let rec convert = function
    | SSL.Var v -> convert_var v
    | SSL.And (f1, f2) -> F.asprintf "(and %s %s)\n" (convert f1) (convert f2)
    | SSL.Or (f1, f2) -> F.asprintf "(or %s %s)\n" (convert f1) (convert f2)
    | SSL.Not f ->  F.asprintf "(not %s)\n" (convert f)
    | SSL.GuardedNeg (f1, f2) ->  F.asprintf "(and %s (not %s))\n" (convert f1) (convert f2)
    | SSL.Star fs -> "(sep " ^ (List.map convert fs |> String.concat " ") ^ ")"
    | SSL.PointsTo (x, LS_t n) -> F.asprintf "(pto %s %s)\n" (convert x) (convert_var n)
    | SSL.Eq [v1; v2] -> F.asprintf "(= %s %s)\n" (convert v1) (convert v2)
    | SSL.Distinct [v1; v2] -> F.asprintf "(distinct %s %s)\n" (convert v1) (convert v2)
    | SSL.Exists (xs, psi) -> F.asprintf "(exists (%s) %s)" (convert_binders xs) (convert psi)
    (* Semantics of predicates is not defined *)
    | SSL.LS (x, y) -> F.asprintf "(ls %s %s)" (convert x) (convert y)
    | SSL.DLS (x, y, f, l) ->
        F.asprintf "(dls %s %s %s %s)" (convert x) (convert y) (convert f) (convert l)
    | SSL.NLS (x, y, z) ->
        F.asprintf "(nls %s %s %s)" (convert x) (convert y) (convert z)

    | other -> raise @@ NotSupported (SSL.node_name other)

  let convert_benchmark phi = F.asprintf "(assert %s)\n(check-sat)" (convert phi)

end

include Convertor_builder.Make(Convertor)
