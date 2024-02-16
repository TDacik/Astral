(* Signature for a generic translation
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Context_sig
open Location_sig
(*open HeapEncoding_sig
*)open SetEncoding_sig
open PredicateEncoding_sig

(*
module type MEMORY_ENCODING = sig

  module Locations : LOCATIONS

  module HeapEncoding : HEAP_ENCODING with module Locations = Locations

end


module type BASE_ENCODING = sig

  include MEMORY_ENCODING

  module Context : CONTEXT with module Locations = Locations
                            and module HeapEncoding = HeapEncoding
end
*)

module type QUANTIFIER_ENCODING = sig

  module Locations : LOCATIONS
  (** Input model of Make functor. *)

  val name : string

  val rewrite : Locations.t -> SMT.Term.t -> SMT.Term.t

end

module type ENCODING = sig

  module Locations : LOCATIONS
  module Context : CONTEXT with module Locations = Locations

  module SetEncoding : SET_ENCODING
  module QuantifierEncoding : QUANTIFIER_ENCODING with module Locations = Locations

  module LS_Encoding : PREDICATE_ENCODING with module Context = Context
  module DLS_Encoding : PREDICATE_ENCODING with module Context = Context
  module NLS_Encoding : PREDICATE_ENCODING with module Context = Context

end
