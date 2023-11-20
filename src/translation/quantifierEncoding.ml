(* Methods for second-order quantifier elimination.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT
open SMT.Range

open Location_sig

module Utils (Locations : LOCATIONS) = struct

  let enumerate_locs xs locs =
    List.map (fun _ ->
      Locations.get_constants locs
    ) xs

  let enumerate connective phi x range =
    List.map (SMT.substitute phi x)
    |> connective

  let enumerate_footprints xs locs =
    let fp_sort = Locations.set_sort locs in
    List.map (fun _ ->
      Locations.powerset locs
      |> List.map (Set.mk_enumeration fp_sort)
    ) xs

end

module Direct (L : LOCATIONS) = struct

  module Locations = L

  let name = "direct"

  let rewrite _ phi =
    SMT.Term.map
      (fun t -> match t with
        | Exists (xs, _, psi) -> Exists (xs, None, psi)
        | Forall (xs, _, psi) -> Forall (xs, None, psi)
        | Exists2 (xs, _, psi) -> Exists (xs, None, psi)
        | Forall2 (xs, _, psi) -> Forall (xs, None, psi)
        | t -> t
      ) phi
end


module Enumeration (L : LOCATIONS) = struct

  module Locations = L
  open Utils(L)

  let name = "enumeration"

  let rewrite context phi =
    SMT.Term.map
      (fun t -> match t with
        | Exists (xs, None, psi) -> Exists (xs, None, psi)
        | Forall (xs, None, psi) -> Forall (xs, None, psi)
        (*| Exists (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (enumerate_locs xs context)
        | Forall (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (enumerate_locs xs context)
        *)
        | Exists (xs, Some (Range ranges), phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs ranges
        | Forall (xs, Some (Range ranges), phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs ranges

          (* Others all not implemented in SMT *)
        | Forall ([x1; x2], Some (Pair ranges), phi) ->
          let fst = BatList.unique ~eq:SMT.equal @@ List.map fst ranges in
          let snd = BatList.unique ~eq:SMT.equal @@ List.map snd ranges in
          BatList.fold_left2 (enumerate Boolean.mk_and) phi [x1; x2] [fst; snd]

        | Exists2 (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (enumerate_footprints xs context)
        | Forall2 (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (enumerate_footprints xs context)
        | Exists2 (xs, Some (Range ranges), phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs ranges
        | Forall2 (xs, Some (Range ranges), phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs ranges
        | t -> t
      ) phi

end

module Path (L : LOCATIONS) = Enumeration(L)

module SmartEnumeration (L : LOCATIONS) = struct

  module Locations = L
  open Utils(L)

  let name = "enumeration"

  let rewrite context phi =
    SMT.Term.map
      (fun t -> match t with
        | Exists (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (enumerate_locs xs context)
        | Forall (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (enumerate_locs xs context)

        | Exists (xs, Some (Range ranges), phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs ranges
        | Forall (xs, Some (Range ranges), phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs ranges

        (* Others all not implemented in SMT *)
        | Forall ([x1; x2], Some (Pair ranges), phi) ->
          ranges
          |> List.map (fun (t1, t2) ->
                let phi' = SMT.substitute phi x1 t1 in
                SMT.substitute phi x2 t2
             )
          |> Boolean.mk_and

        | Exists2 (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (enumerate_footprints xs context)
        | Forall2 (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (enumerate_footprints xs context)
        | Exists2 (xs, Some (Range ranges), phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs ranges
        | Forall2 (xs, Some (Range ranges), phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs ranges
        | t -> t
      ) phi

end
