(* Conversion between precise and imprecise semantics of (dis-)equalities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)


(** Precise -> Imprecise *)

let to_imprecise_sh phi =
  let operands = match phi with
    | SSL.Star psis -> psis
    | psi -> [psi]
  in
  let pure, spatial = List.partition SSL.is_pure (operands) in
  SSL.mk_and [SSL.mk_and pure; SSL.mk_star spatial]

let to_imprecise_arbitrary phi =
  SSL.map (fun psi -> if SSL.is_pure psi then SSL.mk_and [psi; SSL.mk_emp ()] else psi) phi

let to_imprecise phi = match SSL.as_query phi with
  | QF_SymbolicHeap_SAT phi -> to_imprecise_sh phi
  | QF_SymbolicHeap_ENTL (lhs, rhs) -> SSL.mk_gneg (to_imprecise_sh lhs) (to_imprecise_sh rhs)
  | _ -> to_imprecise_arbitrary phi

(** Imprecise -> Precise *)

let to_precise_sh phi =
  SSL.map
    (fun psi -> match psi with
      | And (psi1, psi2) ->
        if SSL.is_pure psi then SSL.mk_star [psi1; psi2]
        else psi
      | psi when SSL.is_pure psi -> SSL.mk_star [psi; SSL.mk_true ()]
      | psi -> psi
    ) phi

let to_precise_arbitrary phi =
  SSL.map (fun psi -> if SSL.is_pure psi then SSL.mk_star [psi; SSL.mk_true ()] else psi) phi

let to_precise phi = match SSL.as_query phi with
  | QF_SymbolicHeap_SAT phi -> to_precise_sh phi
  | QF_SymbolicHeap_ENTL (lhs, rhs) -> SSL.mk_gneg (to_precise_sh lhs) (to_precise_sh rhs)
  | _ -> to_precise_arbitrary phi
