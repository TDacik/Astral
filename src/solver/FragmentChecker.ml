(* Check whether formula lies inside supported fragment.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Context

let check_low_level_sl ctx =
  if SL.is_low_level ctx.phi && not @@ HeapSort.is_bitvector_model ctx.raw_input.heap_sort then
    Result.error
      "Low-level SL (with begin/end operations) defined over sort different than bitvectors"
  else Result.ok ()

let check ctx = check_low_level_sl ctx
