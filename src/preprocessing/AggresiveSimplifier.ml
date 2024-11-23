(* Simplification rules that do not preserve models
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let check g lhs_vars phi = failwith "TODO: Asimplifier: check"
(*  let root = SL.get_root phi in
  | LS (Var x, Var y) ->
    not @@ BatList.mem_cmp SL.Variable.compare x lhs_vars
    && SL_graph.must_neq g y (SL.Variable.nil)
  | PointsTo (Var x, _) ->
    not @@ BatList.mem_cmp SL.Variable.compare x lhs_vars
*)


let simplify g phi = failwith "TODO: Asimplifier: simplify"


(*match SL.view phi with
  | GuardedNeg (lhs, rhs) ->
    let lhs_vars = SL.get_vars lhs in
    let lists = SL.filter (function SL.LS _ | SL.PointsTo _ -> true | _ -> false) rhs in
    if List.exists (check g lhs_vars) lists
    then (lhs, SL.get_vars lhs)
    else (phi, SL.get_vars phi)
  | _ -> (phi, SL.get_vars phi)

*)
