(* Preprocessing that removes unecessary variables originating from equalities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

(** Computation of connected components for equalities.
module Components = Graph.Components.Undirected(SL_graph)

(** Get representant of equivalence classes as the first node in SCC. If SCC contains nil,
    then return nil instead. *)
let get_representant components x =
  try
    let c = List.find (fun c -> BatList.mem_cmp SL.Variable.compare x c) components in
    if BatList.mem_cmp SL.Variable.compare SL.Variable.nil c
    then SL.Variable.nil
    else List.hd c

  with Not_found -> x
*)

let apply sl_graph phi = failwith "TODO: equality rewritter"
(*
  let g = SL_graph.projection_eq sl_graph in
  let components = Components.components_list g in
  let rewrite_var = (fun var -> SL.Var (get_representant components var)) in
  SL.map_vars rewrite_var phi

*)

