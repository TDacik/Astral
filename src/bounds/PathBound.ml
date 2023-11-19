(* Computation of bounds on paths in SL-graphs
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SL_graph

module VarSet = SSL.Variable.Set

exception NoPath

module M = Map.Make(struct

  type t = SSL.Variable.t * SSL.Variable.t * SSL.Field.t [@@deriving compare]

end)

let bound_cache = ref M.empty

let cache_add (x, y, f) bound =
  bound_cache := M.add (x, y, f) bound !bound_cache

let cache_find (x, y, f) =
  M.find (x, y, f) !bound_cache


let rec path_lower_bound g field x y =
  try fst @@ cache_find (x, y, field) with Not_found ->
  let rec traverse g current visited =
    if not @@ must_neq g current y then 0
    else if VarSet.mem current visited then raise NoPath
    else try
      let e = must_successor_edge g field current in
      match E.label e with
      | Pointer _ -> 1 + traverse g (E.dst e) (VarSet.add current visited)
      | Path _ ->
          if must_disjoint g field (E.src e) (E.dst e) y || must_eq g y SSL.Variable.nil then
            path_lower_bound (remove_edge_e g e) field (E.src e) (E.dst e)
            + traverse g (E.dst e) (VarSet.add current visited)
          else 0
    with Invalid_argument _ | Failure _ -> 1 (* TODO? *)
  in
  (* Start recursive path search if x \mustneq y *)
  let g =
    try remove_edge_e g (x, Path field, y)
    with Invalid_argument _ -> g
  in
  traverse g x VarSet.empty

let rec path_upper_bound g field x y max =
  try snd @@ cache_find (x, y, field) with Not_found ->
  let rec traverse current visited =
    if must_eq g current y then 0
    else if VarSet.mem current visited then 0
    else try
      let e = must_successor_edge g field current in
      let weight = match E.label e with
        | Pointer _ -> 1
        | Path _ -> path_upper_bound (remove_edge_e g e) field (E.src e) (E.dst e) max
      in
      weight + traverse (E.dst e) (VarSet.add current visited)
    with Invalid_argument _ | Failure _ -> max
  in
  (* Start recursive search *)
  traverse x VarSet.empty

let partial_path g field x y =
  let rec take current n =
    if n = 0 then []
    else
      try current :: take (must_successor_ptr g field current) (n - 1)
      with Invalid_argument _ | Failure _ -> []
  in
  let prefix_len = path_lower_bound g field x y in
  take x (prefix_len + 1)

let root_weight g x =
  if must_pointer_any g x then 1
  else Float.to_int @@ Float.floor @@ LocationBounds.chunk_size_small x
  (* TODO: can be make general if not SH-entailment *)

let alloc_bound g field x y max =
  must_disjoint_with g x field y
  |> List.filter (fun x' ->
        Sort.equal (SSL.Variable.get_sort x) Sort.loc_nls (* TODO *)
        || Sort.equal (SSL.Variable.get_sort x) (SSL.Variable.get_sort x')
      )
  |> List.filter (fun x' -> not @@ SSL.Variable.equal x x')
  |> List.map (fun x -> root_weight g x)
  |> BatList.sum
  |> (fun x -> max - x)

let compute g field x y max =
  try
    let max =
      if must_path g field x y then alloc_bound g field x y max
      else max
    in
    if max <= 0 then (0, 0)
    else begin
      let lower = path_lower_bound g field x y in
      let upper = min max (path_upper_bound g field x y max) in
      if lower > upper then (0, 0) (* TODO: raise contradiction? *)
      else
        let _ = cache_add (x, y, field) (lower, upper) in
        (lower, upper)
    end
  with NoPath -> (0, 0)
