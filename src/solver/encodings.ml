(* Definition of encodings
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

(** Default encoding *)
module Encoding = struct
  module Set = Z3Sets
  module Locations = QuantifierLocations.Locations(Set)
  open ListEncoding.Make(Set)(Locations)
  module ListEncoding = Classic
end

(** Encoding optimized for symbolic heaps *)
module EncodingSH = struct
  module Set = Z3Sets
  module Locations = QuantifierLocations.Locations(Set)
  open ListEncoding.Make(Set)(Locations)
  module ListEncoding = SymbolicHeaps
end

module TranslationN = Translation.Make(Encoding)
module TranslationSH = Translation.Make(EncodingSH)
