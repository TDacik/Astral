(* Simplification rules that do not preserve models
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL

let check g lhs_vars = function
  | LS (Var x, Var y) ->
    not @@ BatList.mem_cmp SSL.Variable.compare x lhs_vars
    && SL_graph.must_neq g y (SSL.Variable.nil)
  | PointsTo (Var x, _) ->
    not @@ BatList.mem_cmp SSL.Variable.compare x lhs_vars



let simplify g phi = match phi with
  | GuardedNeg (lhs, rhs) ->
    let lhs_vars = SSL.get_vars lhs in
    let lists = SSL.select_subformulae (function SSL.LS _ | SSL.PointsTo _ -> true | _ -> false) rhs in
    if List.exists (check g lhs_vars) lists
    then (lhs, SSL.get_vars lhs)
    else (phi, SSL.get_vars phi)
  | _ -> (phi, SSL.get_vars phi)
