(* Conversion to the input format of the HARRSH tool
 * (https://github.com/katelaan/harrsh).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Struct

open Context
open Convertor_utils


module F = Format

module Convertor = struct
  let name = "Harrsh"
  let suffix = ".hrs"

  let supports_sat = false
  let supports_variadic_operators = false
  let precise_semantics = true (* ? *)

  let global_decls_str () =
    let ls1 = "ls <= emp : {x1 = x2}" in
    let ls2 = "ls <= x1 -> z * ls(z, x2) : {x1 != x2}" in
    let ls = F.asprintf "  %s;\n  %s;" ls1 ls2  in

    let dls1 = "dls <= emp : {x1 = x3, x2 = x4}" in
    let dls2 = "dls <= x1 -> (x3, z) * dls(z, x2, x1, x4) : {x1 != x3, x2 != x4} " in
    let dls = F.asprintf "  %s;\n  %s;" dls1 dls2  in

    let nls1 = "nls <= emp : {x1 = x2}" in
    let nls2 = "nls <= x1 -> (t, n) * ls(n, x3) * nls(t, y, z) : {x1 != x2}" in
    let nls = F.asprintf "  %s;\n  %s" nls1 nls2  in

    ls ^ dls ^ nls

  let global_decls _ = global_decls_str ()

  (** Harrsh does not declare variables *)
  let declare_sort sort = ""
  let declare_var var = ""

  let comment_prefix = "#"

  let set_status context =
    let status = match Context.show_expected_status context with
      | "sat" -> "false"
      | "unsat" -> "true"
      | "unknown" -> "unknown"
      | _ -> assert false
    in
    F.asprintf "info {\nstatus = %s\n}\n" status

  let convert_var var = SSL.Variable.show var

  let rec convert_spatial phi =
    let convert = convert_spatial in
    match phi with
    | phi when SSL.is_true phi -> "true"
    | phi when SSL.is_false phi -> "false"
    | SSL.Emp -> "emp"
    | SSL.Var v -> convert_var v
    | SSL.And (f1, f2) -> F.asprintf "%s & %s" (convert f1) (convert f2)
    | SSL.LS (v1, v2) -> F.asprintf "ls(%s, %s)" (convert v1) (convert v2)
    | SSL.DLS (x, y, f, l) ->
      F.asprintf "dls(%s, %s, %s, %s)" (convert x) (convert y) (convert f) (convert l)
    | SSL.NLS (x, y, z) ->
      F.asprintf "nls(%s, %s, %s)" (convert x) (convert y) (convert z)
    | SSL.PointsTo (x, LS_t n) -> F.asprintf "%s -> %s" (convert x) (convert_var n)
    | SSL.PointsTo (x, DLS_t (n, p)) ->
      F.asprintf "%s -> %s, %s" (convert x) (convert_var n) (convert_var p)
    | SSL.PointsTo (x, NLS_t (t, n)) ->
      F.asprintf "%s -> %s, %s" (convert x) (convert_var t) (convert_var n)

     | SSL.Exists ([x], psi) -> F.asprintf "\\E %s. %s" (convert x) (convert psi)

    | _ -> raise @@ NotSupported (SSL.node_name phi)

  let rec convert_pure (pure_list : SSL.t list) =
    let convert = function
      | SSL.Eq [v1; v2] -> F.asprintf "%s = %s" (convert_spatial v1) (convert_spatial v2)
      | SSL.Distinct [v1; v2] -> F.asprintf "%s != %s" (convert_spatial v1) (convert_spatial v2)
      | _ -> assert false
    in
    let pure =
      List.map convert pure_list
      |> String.concat ", "
    in
    F.asprintf "{%s}" pure

  let rec convert phi =
    if SSL.is_true phi then "true"
    else if SSL.is_false phi then "false"
    else
      let pure_list, spatial_list = SSL.as_symbolic_heap @@ Simplifier.fold_stars phi in
      let pure = convert_pure pure_list in
      let spatial = List.map convert_spatial spatial_list in
      let spatial = String.concat " * " spatial in
      match spatial_list, pure_list with
      | [], [] -> "emp"
      | [], _ -> F.asprintf "emp : %s" pure
      | _, [] -> F.asprintf "%s" spatial
      | _, _ -> F.asprintf "%s : %s" spatial pure

  let formula_to_sid phi tag =
    let phi' = Utils.to_index_vars ~prefix:"x" ~start:1 phi in
    let vars = SSL.get_vars ~with_nil:false phi in
    let sid = F.asprintf "%s <= %s" tag (convert phi') in
    let phi = F.asprintf "%s(%s)" tag (List.map SSL.Variable.show vars |> String.concat ", ") in
    (phi, sid)

  let convert_benchmark phi =
    let lhs, rhs = match phi with
      | SSL.GuardedNeg (lhs, rhs) -> lhs, rhs
      | phi -> phi, SSL.mk_false ()
    in
    let lhs, lhs_sid = formula_to_sid lhs "lhs" in
    let rhs, rhs_sid = formula_to_sid rhs "rhs" in
    F.asprintf "query {\n  %s |= %s\n}\n\n" lhs rhs
    ^ F.asprintf "sid {\n  %s;\n  %s;\n  %s\n}" lhs_sid rhs_sid (global_decls_str ())

  let convert_info input =
    let status = match Context.show_expected_status input with
      | "sat" -> "false"
      | "unsat" -> "true"
      | "unknown" -> "unknown"
      | _ -> assert false
    in
    F.asprintf "info {\n  status = %s\n}\n" status

end

include Convertor

let convert context =
  let phi =
    SSL.normalise
    @@ Utils.to_index_vars ~prefix:"x" ~start:1 context.phi
  in
  let query = convert_benchmark phi in
  Format.asprintf "%s\n\n%s"
    (query)
    (convert_info context)

let dump path input =
    let converted = convert input in
    let target_name = Str.global_replace (Str.regexp "\\.smt2") ".hrs" path in
    let channel = open_out_gen [Open_creat; Open_wronly] 0o666 target_name in
    Printf.fprintf channel "%s" converted;
    close_out channel
