(* Representation of heap encoding.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Location_sig
open HeapEncoding_sig

module Make (L : LOCATIONS) : HEAP_ENCODING with type Locations.t = L.t
