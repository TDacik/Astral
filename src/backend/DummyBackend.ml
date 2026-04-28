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

  let error_not_available () =
    let reason = Format.sprintf "Backend %s is not available" P.name in
    Exceptions.internal_error ~reason ~details:""

  let init ?(timeout=0) _ = error_not_available ()
  let translate _ = error_not_available ()
  let solve _ = error_not_available ()
  let simplify _ = error_not_available ()
  let show_formula _ = error_not_available ()
  let show_model _ = error_not_available ()
  let to_smtlib _ = error_not_available ()

  let push _ = error_not_available ()
  let pop _ = error_not_available ()
  let check_sat _ = error_not_available ()

end
