(* Conversion to the input format of Astral itself.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Context
open Convertor_utils

module F = Format

module Convertor = struct
  let name = "astral"
  let suffix = ".smt2"

  let supports_sat = true
  let supports_variadic_operators = true
  let precise_semantics = true

  let comment_prefix = ";;"

  let global_decls context = ""

  let set_status context =
    F.asprintf "(set-info :status %s)" (Context.show_expected_status context)

  let declare_sort sort = ""
  let declare_var var = ""
  let convert_var var = ""

  let convert phi = ""

  let convert_benchmark = SSLDumper.translate_all

end

include Convertor_builder.Make(Convertor)
