(* Encoding of bounded reachability.
 *
 * TODO: move path quantifiers here.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT
open Encoding_context_sig

module Make (E : Translation_sig.ENCODING) = struct

  module Locations = E.Locations

  (* ==== Reachability ==== *)

  let reach_n selector source sink n =
    let nth_succ = Array.mk_nary_select n selector source in
    SMT.mk_eq [nth_succ; sink]

  let reach heap source sink (min, max) =
    BatList.range min `To max
    |> List.map (reach_n heap source sink)
    |> Boolean.mk_or


  (* ==== Paths ==== *)

  let combine ctx = function
    | [] -> Sets.mk_empty ctx.fp_sort
    | xs ->
      let sort = SMT.get_sort @@ List.hd xs in
      if Sort.is_atomic sort then Sets.mk_constant ctx.fp_sort xs
      else Sets.mk_union ctx.fp_sort xs

  let path_n ctx selector constructor source n =
    if n == 0 then Sets.mk_empty ctx.fp_sort
    else
      BatList.range 0 `To (n - 1)
      |> List.map (fun n -> constructor n @@ Array.mk_nary_select n selector source)
      |> combine ctx

  let path_cons ctx selector constructor source sink (min, max) =
    let cases =
      BatList.range min `To max
      |> BatList.map (fun n ->
          reach_n selector source sink n,
          path_n ctx selector constructor source n
        )
    in
    Boolean.mk_multiple_ite cases (Sets.mk_empty ctx.fp_sort)

  (* Path with identity constructor *)
  let path ctx selector source sink bounds =
    path_cons ctx selector (fun _ x -> x) source sink bounds

  let typed_reach ctx selector source sink typ (min, max) =
    BatList.range min `To max
    |> List.map (fun n -> reach_n selector source sink n, n)
    |> List.map (fun (p, n) -> Boolean.mk_and
      [p;
       Locations.mk_set_of_type ctx.locs (path_n ctx selector (fun _ x -> x) source n) typ
      ])
    |> Boolean.mk_or

end
