(* Native cvc5 backend
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

open Backend_sig
open Encoding_context_sig

module Logger = Logger.MakeWithDir (struct
  let name = "Backend:cvc5"
  let level = 1
  let dirname = "unfolding_queries"
end)

(** Generative module prevents initialization of cvc5 when it is not used *)
module Init ( ) = struct

  (* === Declarations === *)

  type formula = unit

  type model = unit

  let name = "cvc5"

  let supports_smtlib_options = true
  let supports_get_info = true
  let supports_sets = true
  let supports_quantifiers = true

  let is_available () = true

  let init ?timeout () = failwith "Not implemented"
  let translate _ = failwith "Not implemented"
  let solve _ _ _ _ = failwith "Not implemented"
  let simplify _ = failwith "Not implemented"
  let show_formula _ = failwith "Not implemented"
  let to_smtlib _ _ _ = failwith "Not implemented"
  let show_model _ = failwith "Not implemented"
  let push _ = failwith "Not implemented"
  let pop _ = failwith "Not implemented"
  let check_sat _ = failwith "Not implemented"

end
