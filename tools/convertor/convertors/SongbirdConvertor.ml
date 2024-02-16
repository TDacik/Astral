(* Conversion to the input format of the Songbird theorem prover
 * (see https://songbird-prover.github.io/).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Struct

open Convertor_utils

module F = Format

module Convertor = struct

  let name = "Songbird"
  let suffix = ".sb"

  let supports_sat = false
  let supports_variadic_operators = false
  let precise_semantics = false

  let global_decls _ =
    let ls ={|
data ls_node {
    ls_node next;
};

pred ls(in,out) :=
  (emp & in=out)
  \/ (exists u. in->ls_node{u} * ls(u,out) & in!=out);
|} in

  let dls = {|
data dls_node {
    dls_node next;
    dls_node prev;
};

pred dls(x, y, f, l) :=
    (emp & x = l & y = f)
    \/ (exists u. x->dls_node{u, f} * dls(u, y, x, l) & x != l & y != f);
|} in
  let nls = {|
data nls_node {
    ls_node next;
    nls_node top;
};

pred nls(x, y, z) :=
    (emp & x = y)
    \/ (exists n. (exists t. x->nls_node{n, t} * ls(n, z) * nls(t, y, z) & x != y));
|} in
   ls ^ dls ^ nls

  (** Songbird does not declare variables *)
  let declare_sort sort = ""
  let declare_var var = ""

  let comment_prefix = "//"

  let set_status context = Format.asprintf "// status: %s" (Context.show_expected_status context)

  let convert_var var = SSL.Variable.show var

  (* TODO: LIA? *)
  let rec convert phi =
    if SSL.is_true phi then "true"
    else if SSL.is_false phi then "false"
    else match phi with
    | SSL.Emp -> "emp"
    | SSL.Var v -> convert_var v
    | SSL.And (f1, f2) ->
      (* Songbird strictly requires SH to be written as spatial & pure, not vice versa *)
      let spatial, other =
        if not @@ SSL.is_pure f1 then (f1, f2)
        else (f2, f1)
      in
      F.asprintf "(%s & %s)" (convert spatial) (convert other)
    | SSL.Or (f1, f2) -> F.asprintf "(%s \\/ %s)" (convert f1) (convert f2)
    | SSL.Not f ->  F.asprintf "(!%s)" (convert f)
    | SSL.GuardedNeg (f1, f2) ->  F.asprintf "(%s & (!%s))" (convert f1) (convert f2)
    | SSL.Star [f1; f2] -> F.asprintf "(%s * %s)" (convert f1) (convert f2)
    | SSL.LS (v1, v2) -> F.asprintf "(ls(%s, %s))" (convert v1) (convert v2)
    | SSL.DLS (x, y, f, l) ->
        F.asprintf "(dls(%s, %s, %s, %s))" (convert x) (convert y) (convert f) (convert l)
    | SSL.NLS (x, y, z) ->
        F.asprintf "(nls(%s, %s, %s))" (convert x) (convert y) (convert z)
    | SSL.PointsTo (x, LS_t n) -> F.asprintf "(%s->ls_node{%s})" (convert x) (convert_var n)
    | SSL.PointsTo (x, DLS_t (n, p)) ->
        F.asprintf "(%s->dls_node{%s, %s})" (convert x) (convert_var n) (convert_var p)
    | SSL.PointsTo (x, NLS_t (t, n)) ->
        F.asprintf "(%s->nls_node{%s, %s})" (convert x) (convert_var n) (convert_var t)
    | SSL.Eq [v1; v2] -> F.asprintf "(%s = %s)" (convert v1) (convert v2)
    | SSL.Distinct [v1; v2] -> F.asprintf "(%s != %s)" (convert v1) (convert v2)
    | SSL.Exists ([x], psi) -> F.asprintf "(exists %s. %s)" (convert x) (convert psi)
    | SSL.Forall ([x], psi) -> F.asprintf "(forall %s. %s)" (convert x) (convert psi)
    | other -> raise @@ NotSupported (SSL.node_name other)

  let convert_benchmark = function
    | SSL.GuardedNeg (lhs, rhs) -> F.asprintf "checkentail %s |- %s;" (convert lhs) (convert rhs)
    | _ -> assert false

end

include Convertor_builder.Make(Convertor)
