(* Elimination of quantifiers in SL formulae. *)

open SSL

let list_inter xs ys =
  let xs = SSL.Variable.Set.of_list xs in
  let ys = SSL.Variable.Set.of_list ys in
  SSL.Variable.Set.elements (SSL.Variable.Set.inter xs ys)

let list_disjoint xs ys =
  List.is_empty @@ list_inter xs ys

let remove_useless phi = 
  let filter_fn = fun psi x -> BatList.mem_cmp SSL.compare x (SSL.free_vars psi) in
  SSL.map (fun phi -> match phi with
    | Exists (xs, psi) -> SSL.mk_exists (List.filter (filter_fn psi) xs) psi
    | Forall (xs, psi) -> SSL.mk_exists (List.filter (filter_fn psi) xs) psi
    | other -> other
  ) phi

let remove_binder sl_graph phi psi x =
  let local_sl_graph = SL_graph.compute psi in
  let sl_graph = SL_graph.normalise @@ SL_graph.union sl_graph local_sl_graph in
  let eq_vars = SL_graph.equivalence_class sl_graph x in
  let free_vars = List.map (fun (SSL.Var x) -> x) (SSL.free_vars phi) in
  let inter = list_inter eq_vars free_vars in
  match inter with
    | [] -> psi, [x]
    | x' :: _ -> SSL.substitute psi ~var:x ~by:x', []

let remove_determined sl_graph phi =
  SSL.map (fun psi -> match psi with
    | Exists (binders, psi) ->
      let psi, xs = List.fold_left (fun (psi, xs) (Var x) ->
        let psi', xs' = remove_binder sl_graph phi psi x in
        psi', xs @ xs'
      ) (psi, []) binders
      in
      SSL.mk_exists (List.map (fun x -> SSL.Var x) xs) psi
    | other -> other
  ) phi

let apply sl_graph phi =
  remove_determined sl_graph phi
  |> remove_useless
