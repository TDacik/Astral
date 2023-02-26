(* Conversion to the input format of the Songbird theorem prover
 * (see https://songbird-prover.github.io/).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Convertor_utils

module F = Format

module Convertor = struct
  let name = "Songbird"
  let suffix = ".sb"

  let supports_sat = false
  let supports_variadic_operators = false
  let precise_semantics = false
  let supports_ls = true
  let supports_dls = true

  let comment_prefix = "//"

  let set_status context = Format.asprintf "// status: %s" (Context.show_expected_status context)

  (** Songbird does not declare variables *)
  let declare_var var = ""

  let convert_var var = SSL.Variable.show var

  let declare_ls = {|
data ls_node {
    ls_node next;
};

pred ls(in,out) :=
  (emp & in=out)
  \/ (exists u. in->ls_node{u} * ls(u,out) & in!=out);
|}

  let declare_dls = {|
data dll_node {
    dll_node next;
    dll_node prev;
};

pred dll(hd,p,tl,n) := hd->dll_node{n,p} & hd=tl
    \/ (exists x. hd->dll_node{x,p} * dll(x,hd,tl,n));
|}

  (* TODO: LIA? *)
  let rec convert phi =
    if SSL.is_true phi then "true"
    else if SSL.is_false phi then "false"
    else match phi with
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
    (* Different order of parameters! : *)
    | SSL.DLS (x, y, f, l) ->
        F.asprintf "(dll(%s, %s, %s, %s))" (convert x) (convert f) (convert y) (convert l)
    | SSL.PointsTo (v1, [v2]) -> F.asprintf "(%s->ls_node{%s})" (convert v1) (convert v2)
    | SSL.PointsTo (x, [p; n]) ->
        F.asprintf "(%s->dll_node{%s, %s})" (convert x) (convert n) (convert p)
    | SSL.Eq [v1; v2] -> F.asprintf "(%s = %s)" (convert v1) (convert v2)
    | SSL.Distinct [v1; v2] -> F.asprintf "(%s != %s)" (convert v1) (convert v2)
    | SSL.Exists ([x], psi) -> F.asprintf "(exists %s. %s)" (convert x) (convert psi)
    | SSL.Forall ([x], psi) -> F.asprintf "(forall %s. %s)" (convert x) (convert psi)
    | other -> raise @@ NotSupported (SSL.node_name other)

  let convert_assert = function
    | SSL.GuardedNeg (lhs, rhs) -> F.asprintf "checkentail %s |- %s;" (convert lhs) (convert rhs)

  let command = ""

end

include Convertor_builder.Make(Convertor)
