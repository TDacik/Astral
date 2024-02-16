(* Yices2 backend for Astral
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Generic_smtlib

module Self = struct
  let name = "Yices2"
  let binary = "yices-smt2"

  let model_option = ""
  let default_options = []

  let parser_implemented = false

  let supports_smtlib_options = false (* ? *)
  let supports_get_info = true (* ? *)

  let supports_sets = false
  let supports_quantifiers = false (* ? *)

  let translate_non_std = translate_std
  and translate_non_std_sort = translate_std_sort

  let declare_non_std_sort = declare_std_sort

end

include Smtlib_backend_builder.Make(Self)
