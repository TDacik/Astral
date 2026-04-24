(* Elimination of heap terms.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

let rec elim_heap_term phi term = match SL.Term.view term with
  | HeapTerm (field, base) ->
    let base' = elim_heap_term phi base in
    let target = SL.find_pto_target phi base' field in
    Option.value ~default:term target
  | _ -> term (* TODO *)

let apply phi =
  assert (SL.is_symbolic_heap phi);
  SL.map_terms (elim_heap_term phi) phi
