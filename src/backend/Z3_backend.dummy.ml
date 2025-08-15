(* Dummy implementation of Z3 backend when Z3 is not available.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

module Init () = struct
  include DummyBackend.Make(struct let name = "Z3" end)
end
