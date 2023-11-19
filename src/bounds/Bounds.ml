(* Representation of computed location and predicate bounds.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open LocationBounds
open LocationBounds.SortBound

module Self = struct

  type t = {
    location_bounds : LocationBounds.t;
    predicate_bounds : PredicateBounds.Entry.t PredicateBounds.t;
  }

  let show self =
    Format.asprintf "Location bounds: %s\nPredicate bounds: %s"
      (LocationBounds.show self.location_bounds)
      (PredicateBounds.show self.predicate_bounds)

end

include Self
include Datatype.Printable(Self)

let empty = {
  location_bounds = LocationBounds.empty;
  predicate_bounds = PredicateBounds.empty;
}

let compute phi sl_graph =
  let location_bounds = LocationBounds.compute phi sl_graph in
  let predicate_bounds = PredicateBounds.compute sl_graph location_bounds phi in
  let location_bounds =
    if LocationBounds.mem Sort.loc_nls location_bounds
       && not @@ LocationBounds.mem Sort.loc_ls location_bounds
    then LocationBounds.add Sort.loc_ls SortBound.zero location_bounds
    else location_bounds
  in
  {
    location_bounds = location_bounds;
    predicate_bounds = predicate_bounds;
  }

let sort_allocated bounds sort = LocationBounds.allocated sort bounds.location_bounds
let sort_total bounds sort = LocationBounds.total sort bounds.location_bounds
let predicate_bound bounds pred = PredicateBounds.find pred bounds.predicate_bounds

let sum bounds =
  LocationBounds.values bounds.location_bounds
  |> BatList.map (fun b -> b.total)
  |> BatList.sum

let to_json bounds =
  LocationBounds.bindings bounds.location_bounds
  |> List.map (fun (sort, bound) -> (Sort.show sort, `String (SortBound.show bound)))
  |> (fun xs -> `Assoc xs)
