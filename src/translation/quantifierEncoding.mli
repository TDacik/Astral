(* Methods for quantifier elimination.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Location_sig
open Translation_sig

module Direct (L : LOCATIONS) : QUANTIFIER_ENCODING with module Locations = L

module Path (L : LOCATIONS) : QUANTIFIER_ENCODING with module Locations = L

module Enumeration (L : LOCATIONS) : QUANTIFIER_ENCODING with module Locations = L

module SmartEnumeration (L : LOCATIONS) : QUANTIFIER_ENCODING with module Locations = L
