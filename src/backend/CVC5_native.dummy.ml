(* Dummy implementation of cvc5 backend when cvc5 is not available.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

module Init () = struct
  include DummyBackend.Make(struct let name = "cvc5" end)
end
