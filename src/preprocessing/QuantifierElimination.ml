(* Elimination of quantifiers in SL formulae. *)

open SL

module Logger = Logger.MakeWithDir (struct
    let name = "Quantifier elimination"
    let level = 2
    let dirname = "qelim"
end)

let list_inter xs ys =
  let xs = SL.Term.Set.of_list xs in
  let ys = SL.Term.Set.of_list ys in
  SL.Term.Set.elements @@ SL.Term.Set.inter xs ys

let list_disjoint xs ys =
  let xs = SL.Variable.Set.of_list xs in
  let ys = SL.Variable.Set.of_list ys in
  SL.Variable.Set.is_empty @@ SL.Variable.Set.inter xs ys

let remove_useless phi =
  let filter_fn = fun psi x -> BatList.mem_cmp SL.Variable.compare x (SL.free_vars psi) in
  SL.map_view (function
    | Exists (xs, psi) -> SL.mk_exists (List.filter (filter_fn psi) xs) psi
    | Forall (xs, psi) -> SL.mk_exists (List.filter (filter_fn psi) xs) psi
  ) phi

let remove_binder sl_graph phi psi x =
  let local_sl_graph = SL_graph.compute psi in
  let sl_graph = SL_graph.normalise @@ SL_graph.union sl_graph local_sl_graph in
  let eq_vars = SL_graph.equivalence_class sl_graph (SL.Term.of_var x) in
  let free_vars = List.map SL.Term.of_var @@ SL.free_vars phi in
  let inter = list_inter eq_vars free_vars in
  match inter with
    | [] -> psi, [x]
    | x' :: _ -> SL.substitute psi ~var:x ~by:x', []

let remove_binder2 sl_graph phi psi (x : SL.Variable.t) =
  let local_g = SL_graph.compute psi in
  Logger.dump SL_graph.G.output_file (SL.Variable.show x ^ ".xdot") local_g;
  let tx = SL.Term.of_var x in
  match SL_graph.must_pred_field local_g tx with
  | None ->
    Logger.debug "Cannot eliminate var: %s\n" (SL.Variable.show x);
    psi, [x]
  | Some (src, field) ->
    Logger.debug "Must predecessor of %s is %s\n" (SL.Variable.show x) (SL.Term.show_with_sort src);
    SL.substitute psi ~var:x ~by:(SL.Term.mk_heap_term field src), []

let remove_determined sl_graph phi =
  SL.map_view (function
    | Exists (vars, psi) ->
      let psi, xs = List.fold_left (fun (psi, xs) x ->
        let psi', xs' = remove_binder sl_graph phi psi x in
        psi', xs @ xs'
      ) (psi, []) vars
      in
      SL.mk_exists xs psi
  ) phi

let remove_determined2 sl_graph phi =
  SL.map_view (function
    | Exists (vars, psi) ->
      let psi, xs = List.fold_left (fun (psi, xs) x ->
        let psi', xs' = remove_binder2 sl_graph phi psi x in
        psi', xs @ xs'
      ) (psi, []) vars
      in
      SL.mk_exists xs psi
  ) phi


let apply sl_graph phi =
  (*remove_determined sl_graph phi
  |>*) remove_determined2 sl_graph phi
  |> remove_useless
