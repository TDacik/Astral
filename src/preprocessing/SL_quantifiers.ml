let apply locs = failwith "TODO: sl quantifiers"

(*SL.map_view (function
  | Exists (xs, psi) ->
      List.map (fun x ->
        List.map (fun l ->
          SL.substitute psi ~var:x ~by:l
        ) locs
      ) xs
      |> List.concat
      |> SL.mk_or
  )
*)
