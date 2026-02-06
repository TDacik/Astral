(*module Key = struct

  type t =
    | Sort of Sort.t
    | Any
    [@@deriving compare]

  let show = function
    | Sort sort -> Sort.show sort
    | Any -> "any"

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Collections(Self)

end*)

include Sort.MonoMap(AstralLib.Int)
let compare = compare Int.compare

let find_or_zero sort self = match find_opt sort self with
  | None -> 0
  | Some b -> b

let is_leq_zero self sort =
  let bound = find_or_zero sort self in
  bound <= 0

let modify op self sort n =
  let old = find_or_zero sort self in
  add sort (op old n) self

let decrease = modify (-)

let increase = modify (+)

let increase_all n = map (fun x -> x + n)

let plus = union (fun _ x y -> Option.some @@ x + y)

let minus = union (fun _ x y -> Option.some @@ x - y)

let max = union (fun _ x y -> Option.some @@ max x y)

let saturate sorts bound =
  Sort.Set.fold (fun sort acc ->
    increase acc sort 1
  ) sorts bound

let of_sorts alloc_sorts all_sorts =
  List.fold_left (fun acc sort ->
    increase acc sort 1
  ) empty alloc_sorts
  |> saturate all_sorts

let of_location_bound loc_bound =
  LocationBounds0.fold (fun sort bound acc ->
    add sort bound.allocated acc
  ) loc_bound empty
