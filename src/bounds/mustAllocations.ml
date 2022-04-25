(* Computation of sets of allocated locations for each subformula
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open LengthGraph

module P = Printer.Make (struct let name = "Must allocs." end)

let rec must_allocations phi g = match SSL.get_arity phi with
  | Atom _ -> begin match phi with
    | Eq _ | Neq _ -> (g, Variable.Set.empty)
    | PointsTo (x, _) -> (g, Variable.Set.singleton x)
    | LS (x, y) ->
        let allocs =
          if LengthGraph.must_nonempty g x y
          then Variable.Set.singleton x
          else Variable.Set.empty
        in (g, allocs)
  end
  (** Do not continue *)
  | Unary _ -> (g, Variable.Set.empty)
  | Binary (psi1, psi2) ->
      let g1, allocs1 = must_allocations psi1 g in
      let g2, allocs2 = must_allocations psi2 g in
      match phi with
      | And _ -> (LengthGraph.inter g1 g2, Variable.Set.union allocs1 allocs2)
      | Or _ -> (LengthGraph.union g1 g2, Variable.Set.inter allocs1 allocs2)
      | GuardedNeg _ -> (g1, allocs1)
      | Septraction _ -> (* TODO *) (g, Variable.Set.empty)
      | Star _ ->
          (* Pairs of variables that do not alias by semantics of star *)
          let g' = BatList.cartesian_product
            (Variable.Set.elements allocs1)
            (Variable.Set.elements allocs2)
          |> List.fold_left
            (fun g (x, y) ->
              P.debug "Derived %s != %s\n" (Variable.show x) (Variable.show y);
              let g = remove (remove g x y Equal) x y EqualCircular in
              remove (remove g y x Equal) y x EqualCircular
            ) (LengthGraph.inter g1 g2)
          in (g', Variable.Set.union allocs1 allocs2)

(* Refine abstraction based on must allocations *)
let refine_graph phi g = fst @@ must_allocations phi g
