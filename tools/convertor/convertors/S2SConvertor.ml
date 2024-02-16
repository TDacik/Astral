(* Conversion to the input format of the Songbird theorem prover
 * (see https://songbird-prover.github.io/).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Struct

open Convertor_utils

module F = Format

module Convertor = struct

  let name = "S2S"
  let suffix = ".ss"

  let supports_sat = true
  let supports_variadic_operators = false
  let precise_semantics = false

  let global_decls _ =
    let ls = " \
      ddata RefSll_t {RefSll_t next;}. \
\
      pred ls<x:RefSll_t,y:RefSll_t> == \
        emp & x = y \
        or (exists u: x::RefSll_t<next = u> * ls(u,y) & x != y)."
    in
    let dls = " \
      ddata RefDll_t {RefDll_t next; RefDll_t prev;}. \
\
      pred dls<x:RefDll_t,y:RefDll_t,f:RefDll_t,l:RefDll_t> == \
        emp & x = l & y = f \
        or (exists u: x::RefDll_t<next = u, prev=f> * dls(u,y,x,l) & x != l & y != f)."
    in
    let nls = " \
      ddata RefNll_t {RefSll_t next; RefNll_t top;}. \
\
      pred nls<x:RefNll_t,y:RefNll_t,z:RefSll_t> == \
        emp & x = y \
        or (exists tx, nx: x::RefNll_t<next = nx, top = tx> * ls(nx,z) * nls(tx,y,z) & x != y)."
    in
    ls ^ dls ^ nls

  (** S2S does not declare variables *)
  let declare_sort sort = ""
  let declare_var var = ""

  let comment_prefix = "//"

  let set_status context = Format.asprintf "// status: %s" (Context.show_expected_status context)

  let convert_var var =
    if SSL.Variable.is_nil var then "null"
    else SSL.Variable.show var

  let rec convert phi =
    if SSL.is_true phi then "true"
    else if SSL.is_false phi then "false"
    else match phi with
    | Emp -> "emp"
    | Var v -> convert_var v
    | And (f1, f2) ->
      (* S2S strictly requires SH to be written as spatial & pure, not vice versa *)
      let spatial, other =
        if not @@ SSL.is_pure f1 then (f1, f2)
        else (f2, f1)
      in
      F.asprintf "%s & %s" (convert spatial) (convert other)
    | Star [f1; f2] -> F.asprintf "%s * %s" (convert f1) (convert f2)
    | LS (v1, v2) -> F.asprintf "ls(%s, %s)" (convert v1) (convert v2)
    | DLS (x, y, f, l) ->
      F.asprintf "dls(%s, %s, %s, %s)" (convert x) (convert y) (convert f) (convert l)
    | NLS (x, y, z) ->
      F.asprintf "nls(%s, %s, %s)" (convert x) (convert y) (convert z)
    | PointsTo (x, LS_t n) ->
      F.asprintf "%s::RefSll_t<next=%s>" (convert x) (convert_var n)
    | PointsTo (x, DLS_t (n, p)) ->
      F.asprintf "%s::RefDll_t<next=%s, prev=%s>" (convert x) (convert_var n) (convert_var p)
    | PointsTo (x, NLS_t (t, n)) ->
      F.asprintf "%s::RefDll_t<next=%s, top=%s>" (convert x) (convert_var n) (convert_var t)
    | Eq [v1; v2] -> F.asprintf "%s = %s" (convert v1) (convert v2)
    | Distinct [v1; v2] -> F.asprintf "%s != %s" (convert v1) (convert v2)
    | Exists ([x], psi) -> F.asprintf "(exists %s. %s)" (convert x) (convert psi)
    | other -> raise @@ NotSupported (SSL.node_name other)

  let convert_benchmark = function
    (* Entailment query *)
    | SSL.GuardedNeg (lhs, rhs) ->
      F.asprintf "checkent[valid] %s |- %s." (convert lhs) (convert rhs)
    (* Satisfiability query *)
    | phi -> F.asprintf "checksat[sat] %s." (convert phi)

end

include Convertor_builder.Make(Convertor)
