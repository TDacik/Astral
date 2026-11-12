(* Output of synthesis problem as a benchmark.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

open Sexplib
open PrintUtils

let definitions () = "TODO: definitions"

let out_inputs inputs =
  List.map (fun psi ->
    let s_exp = SL.to_sexp psi in
    let s_exp =
      Sexp.List [
        Sexp.Atom "assert";
        Sexp.List [
          Sexp.Atom "!";
          s_exp;
          Sexp.Atom ":positive-example"]]
    in
    Sexp.to_string_hum s_exp
  ) inputs
  |> String.concat "\n\n"

let output phi =
  Sexp.to_string_hum (Sexp.List [Sexp.Atom "assert"; SL.to_sexp phi])

let declare_var var =
  Format.asprintf "(declare-const %s)" (SL.Variable.smt2_decl var)

let declare_vars vars = String.concat "\n" @@ List.map declare_var vars

let out (ctx : ParserContext.t) inputs =
  SMTLIB_printer.generate_definitions (SL.mk_and inputs) ctx.heap_sort
  +++ (declare_vars @@ ParserContext.get_vars ctx)
  +++ (out_inputs inputs)
  +++ "(compute-abstraction)"

let out_predicates preds = String.concat "\n" @@ List.map InductiveDefinition.smt2_decl preds

let out_res (ctx : ParserContext.t) phi =
  ";; Result of the synthesis"
  +++ SMTLIB_printer.generate_definitions phi ctx.heap_sort
  +++ (declare_vars @@ ParserContext.get_vars ctx)
  +++ (output phi)
  +++ "(check-sat)"
