(* Definitions of encodings.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)


(** Direct encoding of sets. *)
module Sets = struct
  module Locations = DatatypeLocations
  module Set = DirectSets
end


(** Encoding of sets using bitvectors. *)
module Bitvectors = struct
  module Locations = BitvectorLocations
  module Set = BitvectorSets
end
