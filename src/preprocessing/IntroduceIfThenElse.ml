(* Introduction of the if-then-else operator instead of disjunctions.
 *
 * TODO: consider n-ary disjunctions?
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

(*


let drop_non_atoms phi = match SL.view phi with
  | Or xs -> SL.mk_or @@ List.filter SL.is_atom xs

let apply =
  SL.map_view (function
    | Or [lhs; rhs] ->
      let pure1, spatial1 = SL.as_symbolic_heap @@ drop_non_atoms lhs in
      let pure2, spatial2 = SL.as_symbolic_heap @@ drop_non_atoms rhs in
      let conflicts =
        BatList.cartesian_product pure1 pure2
        |> List.filter (fun (x1, x2) -> is_conflict x1 x2)
      in
      if List.is_empty conflicts
      then SL.mk_or [lhs; rhs] (* No change *)
      else
        let p1 = List.filter (fun x -> not @@ BatList.mem_cmp SL.compare x @@ List.map fst conflicts) pure1 in
        let p2 = List.filter (fun x -> not @@ BatList.mem_cmp SL.compare x @@ List.map snd conflicts) pure2 in
        let lhs = SL.mk_star (p1 @ spatial1) in
        let rhs = SL.mk_star (p2 @ spatial2) in
        SL.mk_ite (SL.mk_or @@ List.map fst conflicts) lhs rhs
  )

*)

let split phi =
  let pure = SL.select_subformulae SL.is_pure phi in
  let spatial = SL.map_view (function Eq _ | Distinct _ -> SL.emp) phi in
  pure, Simplifier.simplify spatial

let is_unsat pure1 pure2 =
  let sl_graph = SL_graph.compute @@ SL.mk_and (pure1 @ pure2) in
  SL_graph.has_contradiction sl_graph

let apply =
  SL.map_view (function
    | Or [lhs; rhs] when List.for_all SL.is_symbolic_heap [lhs; rhs] ->
      let pure1, spatial1 = split lhs in
      let pure2, spatial2 = split rhs in
      if is_unsat pure1 pure2 then SL.mk_ite (SL.mk_and pure1) spatial1 spatial2
      else SL.mk_or [lhs; rhs]
  )
