(* Preprocessing that removes unecessary variables
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

(** Computation of connected components for equalities. *)

module Components = Graph.Components.Undirected(SL_graph)

(** Get representant of equivalence classes as the first node in SCC. *)
let get_representant components x =
  try
    let c = List.find (fun c -> BatList.mem_cmp SSL.Variable.compare x c) components in
    if BatList.mem_cmp SSL.Variable.compare SSL.Variable.nil c
    then SSL.Variable.nil
    else List.hd c

  with Not_found -> x

let preprocess sl_graph phi =
  let g = SL_graph.projection_eq sl_graph in
  let components = Components.components_list g in
  let rewrite_var = (fun var -> SSL.Var (get_representant components var)) in
  SSL.map_vars rewrite_var phi

