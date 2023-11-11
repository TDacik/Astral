(* Conversion to the input format of the GRASShopper tool (https://github.com/wies/grasshopper).
 *
 * Since GRAShopper is a verification tool rather than solver, the input formula is translated
 * to an equivalent verification problem of an empty program.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Convertor_sig

module Convertor : CONVERTOR_BASE

include CONVERTOR
