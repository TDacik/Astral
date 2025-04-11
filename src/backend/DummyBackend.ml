(* Builder for dummy backend implementations.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

module type PARAMS = sig
  val name : string
end

module Make (P : PARAMS)  = struct

  type formula = |
  type model = |

  let name = P.name
  let is_available () = false

  let supports_smtlib_options = false
  let supports_get_info = false
  let supports_sets = false
  let supports_quantifiers = false

  let init ?(timeout=0) _ = failwith "Not available"
  let translate _ = failwith "Not available"
  let solve _ = failwith "Not available"
  let simplify _ = failwith "Not available"
  let show_formula _ = failwith "Not available"
  let show_model _ = failwith "Not available"
  let to_smtlib _ = failwith "Not available"

  let push _ = failwith "Not available"
  let pop _ = failwith "Not available"
  let check_sat _ = failwith "Not available"

end
