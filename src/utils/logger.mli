(* Interface for printing and debugging
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Logger_sig

module Make (C : CONFIG) : LOGGER
