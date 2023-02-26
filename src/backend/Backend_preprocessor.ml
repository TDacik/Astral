(* Preprocessing of higher-level constructs in internal SMT representation
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SMT

let unfold_disjoint (s :: sets) =
  snd @@ List.fold_left
    (fun (t, phi) set ->
      let t' = Set.mk_union [t; set] (SMT.get_sort set) in
      let phi' = Set.mk_disjoint t set in
      (t', Boolean.mk_and [phi; phi'])
    ) (s, Boolean.mk_true ()) sets

let apply term =
  SMT.map (function
    | Disjoint sets -> unfold_disjoint sets
    | t -> t
  ) term

