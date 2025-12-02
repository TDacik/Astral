open Backend_sig
open Location_sig
open SetEncoding_sig
open Translation_sig
open Encoding_context_sig

let get_location_encoding () = match Config.LocationEncoding.get () with
  | `Datatype -> (module DatatypeLocations : LOCATIONS)
  | `Bitvectors -> (module BitvectorLocations : LOCATIONS)

let get_set_encoding () = match Config.SetEncoding.get () with
  | `Direct -> (module DirectSets : SET_ENCODING)
  | `Bitvectors -> (module BitvectorSets : SET_ENCODING)

let get_backend_aux = function
  | `Auto -> BackendSelection.auto_select ()
  | `Bitwuzla -> (module BitwuzlaNative.Init() : BACKEND)
  | `Bitwuzla_ext -> (module Bitwuzla_backend : BACKEND)
  | `Boolector -> (module Boolector_backend : BACKEND)
  | `cvc5 -> (module CVC5_backend : BACKEND)
  | `Yices2 -> (module Yices_backend : BACKEND)
  | `Z3 -> (module Z3_backend.Init() : BACKEND)
  | `Z3_ext -> (module Z3_external : BACKEND)

(* TODO: backend options should be also handled here. *)

let get_backend () = get_backend_aux @@ Config.Backend.get ()
let get_incremental_backend () = get_backend_aux @@ Config.Backend.get ()

let get_encoding () =
  let module L = (val get_location_encoding () : LOCATIONS) in
  let module S = (val get_set_encoding () : SET_ENCODING) in
  let module H = ArrayEncoding.Make(L) in
  let module Q = (val match Config.QuantifierEncoding.get () with
    | `Direct -> (module QuantifierEncoding.Direct(L) : QUANTIFIER_ENCODING with type Locations.internal = L.internal)
    | `Enum -> (module QuantifierEncoding.Enumeration(L) : QUANTIFIER_ENCODING with type Locations.internal = L.internal))
  in
  let (module C) = (module Translation_context.Make(L)(H) : ENCODING_CONTEXT
    with type Locations.internal = L.internal
     and type HeapEncoding.t = H.t
     and type HeapEncoding.Locations.internal = L.internal
     and type t = (L.t, H.t) Encoding_context_sig.t
  )
  in
  (module struct
    module Locations = L
    module HeapEncoding = H
    module Context = C
    module SetEncoding = S
    module QuantifierEncoding = Q
  end : ENCODING)
