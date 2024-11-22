(* Preprocessing of higher-level constructs in internal SMT representation
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SMT

let unfold_disjoint (s :: sets) =
  snd @@ List.fold_left
    (fun (t, phi) set ->
      let t' = Sets.mk_union (SMT.get_sort set) [t; set] in
      let phi' = Sets.mk_disjoint [t; set] in
      (t', Boolean.mk_and [phi; phi'])
    ) (s, Boolean.tt) sets

let apply term =
  SMT.map_view (function
    | Disjoint sets -> unfold_disjoint sets
  ) term

