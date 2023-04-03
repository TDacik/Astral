(* Conversion to the input format of the cvc5 solver
 * (see https://cvc5.github.io/docs/cvc5-1.0.2/theories/separation-logic.html).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Convertor_sig

module Convertor : CONVERTOR_BASE

include CONVERTOR
