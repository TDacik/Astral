(* Signature for a generic translation
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Location_sig
open SetEncoding_sig
open HeapEncoding_sig
open Encoding_context_sig

module type QUANTIFIER_ENCODING = sig

  module Locations : LOCATIONS
  (** Input model of Make functor. *)

  val name : string

  val rewrite : Locations.t -> SMT.t -> SMT.t

end

module type ENCODING = sig

  module Locations : LOCATIONS
  module HeapEncoding : HEAP_ENCODING with module Locations = Locations

  module Context : ENCODING_CONTEXT
    with module Locations = Locations
    with module HeapEncoding = HeapEncoding
     and type t = (Locations.t, HeapEncoding.t) Translation_context.t

  module SetEncoding : SET_ENCODING
  module QuantifierEncoding : QUANTIFIER_ENCODING with module Locations = Locations

end
