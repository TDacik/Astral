(* Dummy implementation of BitwuzlaNative backend when bitwuzla is not available.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

module Init () = struct
  include DummyBackend.Make(struct let name = "Bitwuzla-native" end)
end
