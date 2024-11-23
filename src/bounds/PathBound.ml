(* Computation of bounds on paths in SL-graphs
 *
 * TODO: check whether caching is really needed
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SL_graph0

module TermSet = SL.Term.Set

module Logger = Logger.Make(struct let name = "Path bound" let level = 2 end)

exception NoPath

module M = Map.Make(struct

  type t = SL.Term.t * SL.Term.t * MemoryModel.Field.t [@@deriving compare]

end)

let bound_cache = ref M.empty

let cache_add (x, y, f) bound =
  bound_cache := M.add (x, y, f) bound !bound_cache

let cache_find (x, y, f) =
  M.find (x, y, f) !bound_cache

let cache_reset () =
  bound_cache := M.empty

let rec path_lower_bound g field x y =
  try fst @@ cache_find (x, y, field) with Not_found ->
  let rec traverse g current visited =
    if not @@ must_neq g current y then 0
    else if TermSet.mem current visited then raise NoPath
    else try
      let e = must_successor_edge g field current in
      match E.label e with
      | Pointer _ -> 1 + traverse g (E.dst e) (TermSet.add current visited)
      | Path _ ->
          if must_disjoint g field (E.src e) (E.dst e) y || must_eq g y SL.Term.nil then
            path_lower_bound (remove_edge_e g e) field (E.src e) (E.dst e)
            + traverse g (E.dst e) (TermSet.add current visited)
          else 0
    with Invalid_argument _ | Failure _ -> 0 (* TODO? *)
  in
  (* Start recursive path search if x \mustneq y *)
  let g =
    try remove_edge_e g (x, Path field, y)
    with Invalid_argument _ -> g
  in
  traverse g x TermSet.empty

let rec path_upper_bound g field x y max =
  try snd @@ cache_find (x, y, field) with Not_found ->
  let rec traverse current visited =
    if must_eq g current y then 0
    else if TermSet.mem current visited then 0
    else try
      let e = must_successor_edge g field current in
      let weight = match E.label e with
        | Pointer _ -> 1
        | Path _ -> path_upper_bound (remove_edge_e g e) field (E.src e) (E.dst e) max
      in
      weight + traverse (E.dst e) (TermSet.add current visited)
    with Invalid_argument _ | Failure _ -> max
  in
  (* Start recursive search *)
  traverse x TermSet.empty

let path_upper_bound g field x y max =
  let aux = path_upper_bound g field x y max in
  min max aux

let partial_path g field x y =
  let rec take current n =
    if n = 0 then []
    else
      try current :: take (must_successor_ptr g field current) (n - 1)
      with Invalid_argument _ | Failure _ -> []
  in
  let prefix_len = path_lower_bound g field x y in
  take x (prefix_len + 1)

let root_weight g phi heap_sort x =
  if must_pointer_any g x then 1
  else Float.to_int @@ Float.floor @@ SID.term_bound phi heap_sort x

let alloc_bound g phi heap_sort field x y max =
  must_disjoint_with g x field y
  |> List.filter (fun x' ->
        (*Sort.equal (SL.get_sort x) Sort.loc_nls (* TODO *)
        ||*) Sort.equal (SL.Term.get_sort x) (SL.Term.get_sort x')
      )
  |> List.filter (fun x' -> not @@ SL.Term.equal x x')
  |> List.map (fun x -> root_weight g phi heap_sort x)
  |> BatList.sum
  |> (fun x -> max - x)

let compute g phi heap_sort field x y max =
  try
    let max =
      if must_path g field x y then alloc_bound g phi heap_sort field x y max
      else max
    in
    if max <= 0 then (0, 0)
    else begin
      let lower = path_lower_bound g field x y in
      let upper = path_upper_bound g field x y max in
      if lower > upper then (0, 0) (* TODO: raise contradiction? *)
      else
        let _ = cache_add (x, y, field) (lower, upper) in
        (lower, upper)
    end
  with NoPath -> (0, 0)
