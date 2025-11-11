(* Dummy implementation of Z3 backend when Z3 is not available.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

let is_available () = false

module Init () = struct
  include DummyBackend.Make(struct let name = "Z3" end)
end

let () = BackendConfig.register_native "z3" ~package:"z3" ~available:false
