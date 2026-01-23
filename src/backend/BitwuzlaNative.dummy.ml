(* Dummy implementation of BitwuzlaNative backend when bitwuzla is not available.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

let is_available () = false

module Init () = struct
  include DummyBackend.Make(struct let name = "Bitwuzla-native" end)
end

let () = BackendConfig.register_native "bitwuzla" ~package:"bitwuzla-cxx" ~available:false
