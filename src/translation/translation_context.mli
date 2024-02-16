(* Translation context
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Context_sig
open Location_sig

module Make (L : LOCATIONS) :
  CONTEXT with module Locations = L
           and module HeapEncoding = HeapEncoding.Make(L)
