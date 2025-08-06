(* List utilities
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let all_equal eq = function
  | [] -> true
  | x :: xs -> List.for_all (eq x) xs

let index_of_cmp cmp x xs =
  BatList.fold_lefti (fun acc i elem -> match acc with
    | Some i -> Some i
    | None -> if cmp x elem = 0 then Some i else None
  ) None xs

(* TODO: fix quadratic complexity *)
let rec all_distinct eq = function
  | [] -> true
  | x :: xs -> List.for_all (fun y -> not @@ eq x y) xs && all_distinct eq xs

let diagonal_product xs =
  BatList.fold_lefti (fun acc i x ->
    BatList.fold_lefti (fun acc j y ->
      if i < j then (x, y) :: acc
      else acc
    ) acc xs
  ) [] xs

(* TODO: name *)
let for_all2' f xs =
  diagonal_product xs
  |> List.for_all (fun (x, y) -> f x y)

let split3 lst =
  List.fold_right (fun (x, y, z) (xs, ys, zs) -> (x :: xs, y :: ys, z :: zs)) lst ([], [], [])

let rec map3 fn xs ys zs = match xs, ys, zs with
  | [], [], [] -> []
  | x :: xs, y :: ys, z :: zs -> fn x y z :: map3 fn xs ys zs
  | _ -> failwith "list lengths differ"

let zip3 xs ys zs =
  map3 (fun x y z -> (x, y, z)) xs ys zs

let rec sublists = function
  | [] -> [[]]
  | x :: xs ->
    let subs = sublists xs in
    subs @ List.map (fun l -> x :: l) subs

module Relation (X : Datatype_sig.COMPARISON) = struct

  module X = struct
    include X
    let hash = Hashtbl.hash
    let equal x y = X.compare x y == 0
  end

  module G = struct
    module G0 = Graph.Persistent.Graph.Concrete(X)
    include G0
    include Graph.Oper.P(G0)
  end

  let transitive_closure_of_graph g =
    let g' = G.transitive_closure g in
    G.fold_edges (fun u v acc -> (u, v) :: acc) g' []

  let transitive_closure xs =
    let g = List.fold_left (fun g (u, v) -> G.add_edge g u v) G.empty xs in
    transitive_closure_of_graph g

  let transitive_closure_list xs =
    let add_edge_list g xs =
      diagonal_product xs
      |> List.fold_left (fun acc (u, v) -> G.add_edge acc u v) g
    in
    let g = List.fold_left (add_edge_list) G.empty xs in
    transitive_closure_of_graph g

end
