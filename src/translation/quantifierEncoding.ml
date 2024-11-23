(* Methods for second-order quantifier elimination.
 *
 * TODO: implement path quantifiers
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT

open Location_sig

module Utils (Locations : LOCATIONS) = struct

  let enumerate_locs xs locs =
    List.map (fun _ -> locs.constants) xs

  let enumerate connective phi x range =
    List.map (fun term -> SMT.substitute phi ~var:x ~by:term) range
    |> connective

  let enumerate_footprints xs locs =
    let fp_sort = Locations.set_sort locs in
    List.map (fun _ ->
      Locations.powerset locs
      |> List.map (Sets.mk_constant fp_sort)
    ) xs

end

module Direct (L : LOCATIONS) = struct

  module Locations = L

  let name = "direct"

  let rewrite _ phi =
    SMT.map_view
      (function
        | Exists (xs, _, psi) | Exists2 (xs, _, psi) -> Quantifier.mk_exists xs psi
        | Forall (xs, _, psi) | Forall2 (xs, _, psi) -> Quantifier.mk_forall xs psi
      ) phi
end


module Enumeration (L : LOCATIONS) = struct

  module Locations = L
  open Utils(L)

  let name = "enumeration"

  let rewrite context phi =
    SMT.map_view
      (function
        | Exists (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (enumerate_locs xs context)
        | Forall (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (enumerate_locs xs context)

        | Exists (xs, Some ranges, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (List.map Lazy.force ranges)
        | Forall (xs, Some ranges, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (List.map Lazy.force ranges)

       (* Others all not implemented in SMT
        | Forall ([x1; x2], Pair ranges, phi) ->
          let fst = BatList.unique ~eq:SMT.equal @@ List.map fst ranges in
          let snd = BatList.unique ~eq:SMT.equal @@ List.map snd ranges in
          BatList.fold_left2 (enumerate Boolean.mk_and) phi [x1; x2] [fst; snd]
        *)
        | Exists2 (xs, None, phi) ->
          Format.printf "TODO 1\n";
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (enumerate_footprints xs context)
        | Forall2 (xs, None, phi) ->
          Format.printf "TODO 2\n";
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (enumerate_footprints xs context)
        | Exists2 (xs, Some ranges, phi) ->
          Format.printf "TODO: %d x %d\n" (List.length xs) (List.length ranges);
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (List.map Lazy.force ranges)
        | Forall2 (xs, Some ranges, phi) ->
          Format.printf "TODO 4\n";
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (List.map Lazy.force ranges)

      ) phi

end

module Path (L : LOCATIONS) = Enumeration(L)

module SmartEnumeration (L : LOCATIONS) = struct

  module Locations = L
  open Utils(L)

  let name = "enumeration"

  let rewrite context phi =
    SMT.map_view
      (function
        | Exists (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (enumerate_locs xs context)
        | Forall (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (enumerate_locs xs context)

        | Exists (xs, Some ranges, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (List.map Lazy.force ranges)
        | Forall (xs, Some ranges, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (List.map Lazy.force ranges)

        (* Others all not implemented in SMT
        | Forall ([x1; x2], Pair ranges, phi) ->
          ranges
          |> List.map (fun (t1, t2) ->
                let phi' = SMT.substitute phi ~var:x1 ~by:t1 in
                SMT.substitute phi ~var:x2 ~by:t2
             )
          |> Boolean.mk_and

        | Exists2 (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (enumerate_footprints xs context)
        | Forall2 (xs, None, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (enumerate_footprints xs context)
        | Exists2 (xs, Range ranges, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_or) phi xs ranges
        | Forall2 (xs, Range ranges, phi) ->
          BatList.fold_left2 (enumerate Boolean.mk_and) phi xs ranges
        *)
      ) phi

end
