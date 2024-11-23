(* Translation context
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Location_sig
open HeapEncoding_sig
open Encoding_context_sig

type nonrec ('locs, 'heap) t = ('locs, 'heap) t

module Make (L : LOCATIONS) (H : HEAP_ENCODING with module Locations = L) : ENCODING_CONTEXT
  with module Locations = H.Locations
   and module HeapEncoding = H
   and type t = (H.Locations.t, H.t) t
