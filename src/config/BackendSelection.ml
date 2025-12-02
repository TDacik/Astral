(* Automatic selection of backend.
 *
 * TODO: consider also input (integers, quantifiers, ....)
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open Backend_sig

let priority_list = [
  `Bitwuzla;
  `Bitwuzla_ext;
  `Z3;
  `Z3_ext;
  `cvc5;
  `Yices2;
  `Boolector;
]

let is_available name = match name with
  | `Bitwuzla -> BitwuzlaNative.is_available ()
  | `Bitwuzla_ext -> Bitwuzla_backend.is_available ()
  | `Boolector -> Boolector_backend.is_available ()
  | `cvc5 -> CVC5_backend.is_available ()
  | `Yices2 -> Yices_backend.is_available ()
  | `Z3 -> Z3_backend.is_available ()
  | `Z3_ext -> Z3_external.is_available ()

let auto_select () =
  match List.find is_available priority_list with
  | `Bitwuzla -> (module BitwuzlaNative.Init() : BACKEND)
  | `Bitwuzla_ext -> (module Bitwuzla_backend : BACKEND)
  | `Boolector -> (module Boolector_backend : BACKEND)
  | `cvc5 -> (module CVC5_backend : BACKEND)
  | `Yices2 -> (module Yices_backend : BACKEND)
  | `Z3 -> (module Z3_backend.Init() : BACKEND)
  | `Z3_ext -> (module Z3_external : BACKEND)
