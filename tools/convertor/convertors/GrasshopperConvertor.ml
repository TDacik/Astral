(* Conversion to the input format of the GRASShopper tool (https://github.com/wies/grasshopper).
 *
 * Since GRASShopper is a verification tool rather than solver, the input formula is translated
 * to an equivalent verification problem of an empty program.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open SSL
open SSL.Node

open Context

open Convertor_utils

module F = Format

module Convertor = struct
  let name = "GRASShopper"
  let suffix = ".spl"

  let supports_sat = false
  let supports_variadic_operators = false
  let precise_semantics = true

  let comment_prefix = "//"

  let global_decls ctx =
    let sorts = SSL.get_loc_sorts ctx.phi in
    if List.mem Sort.loc_dls sorts then"
struct Node {
  var next: Node;
  var prev: Node;
}

predicate dlseg(x1: Node, x2: Node, y1: Node, y2: Node) {
  acc({ z: Node :: Btwn(next, x1, z, y1) && z != y1}) &*&
   (x1 == y1 && x2 == y2 ||
   //x1 in FP && y2 in FP &&
   x1 != x2 && y1 != y2 &&
   Btwn (next, x1, y2, y1) &&
   Btwn (prev, y2, x1, x2) &&
   //y2.next == y1 &&
   (forall l1: Node :: Btwn(next, y2, l1, y1) ==> l1 == y2 || l1 == y1) &&
   //x1.prev == x2 &&
   (forall l1: Node :: Btwn(prev, x1, l1, x2) ==> l1 == x1 || l1 == x2) &&
   (forall l1: Node, l2: Node ::
     Btwn(next, x1, l1, y1) && Btwn(next, x1, l2, y1) && Btwn(next, l1, l2, y1) ==>
       l2 == y1 || Btwn(prev, y2, l2, l1) && Btwn(prev, l2, l1, x1)) &&
   (forall l1: Node, l2: Node ::
     Btwn(prev, y2, l1, x2) && Btwn(prev, y2, l2, x2) && Btwn(prev, l1, l2, x2) ==>
       l2 == x2 || Btwn(next, x1, l2, l1) && Btwn(next, l2, l1, y1)))
}

"
    else
    " \
struct Node { \n\
  var next: Node; \n\
}\n \
\n \
predicate lseg(x: Node, y: Node) {\n \
  acc({ z: Node :: Btwn(next, x, z, y) && z != y }) &*& Reach(next, x, y)\n \
}\n \
"

  let set_status context = Format.asprintf "// status: %s" (Context.show_expected_status context)

  let declare_sort _ = ""

  (** Variables are declared in query *)
  let declare_var _ = ""

  let convert_var var =
    if SSL.Variable.is_nil var then "null"
    else SSL.Variable.show var

  let rec convert = function
    | SSL.Var v -> convert_var v
    | SSL.Emp -> "(null = null)"
    | SSL.Pure term -> failwith "TODO: pure"
    | SSL.And (f1, f2) -> F.asprintf "(%s && %s)" (convert f1) (convert f2)
    | SSL.Or (f1, f2) -> F.asprintf "(%s || %s)" (convert f1) (convert f2)
    | SSL.Not f ->  F.asprintf "(!%s)\n" (convert f)
    | SSL.Star [f1; f2] ->  F.asprintf "(%s &*& %s)" (convert f1) (convert f2)
    | SSL.LS (v1, v2) -> F.asprintf "(lseg (%s, %s))" (convert v1) (convert v2)
    | SSL.DLS (x, y, px, ny) -> F.asprintf "(dlseg (%s, %s, %s, %s))"
      (convert x) (convert px) (convert ny) (convert y)
    | SSL.PointsTo (x, LS_t n) -> F.asprintf "(%s.next |-> %s)" (convert x) (convert_var n)
    | SSL.PointsTo (x, DLS_t (n, p)) ->
      F.asprintf "(acc(%s) &*& %s.next = %s &*& %s.prev = %s)"
        (convert x) (convert x) (convert_var n) (convert x) (convert_var p)
    | SSL.Eq [v1; v2] -> F.asprintf "(%s == %s)" (convert v1) (convert v2)
    | SSL.Distinct [v1; v2] -> F.asprintf "(%s != %s)" (convert v1) (convert v2)
    | SSL.GuardedNeg (f1, f2) -> F.asprintf "(%s && (!%s))" (convert f1) (convert f2)
    | other -> raise @@ NotSupported (SSL.node_name other)

  (** Convert query F |= G to {F} skip; {G} *)

  let procedure_header_var var =
    let sort = SSL.Variable.get_sort var in
    if Sort.is_ls sort then F.asprintf "%a : Node" SSL.Variable.pp var
    else if Sort.is_dls sort then F.asprintf "%a : Node" SSL.Variable.pp var
    else failwith "Not supported"

  let procedure_header phi =
    SSL.get_vars ~with_nil:false phi
    |> List.map procedure_header_var
    |> String.concat ", "

  let convert_benchmark phi = match phi with
    | SSL.GuardedNeg (pre, post) ->
      F.asprintf "procedure query(%s)\n  requires %s\n  ensures %s\n {}"
        (procedure_header phi)
        (convert pre)
        (convert post)
    | _ -> assert false

end

include Convertor_builder.Make(Convertor)
