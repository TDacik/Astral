(* High-level access to command-line options.
 *
 * This module needs to be factored out of Options_base to prevent circular dependencies.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Backend_sig
open Location_sig
open Context_sig
open SetEncoding_sig
open HeapEncoding_sig
open Translation_sig

module Options = Options_base

let backend () = match Options.backend () with
  | "bitwuzla" -> (module Bitwuzla_backend : BACKEND)
  | "boolector" -> (module Boolector_backend : BACKEND)
  | "cvc5" -> (module CVC5_backend : BACKEND)
  | "z3" -> (module Z3_backend.Init( ) : BACKEND)
  | "yices2" -> (module Yices_backend : BACKEND)
  | "auto" ->
    if Bitwuzla_backend.is_available () then (module Bitwuzla_backend : BACKEND)
    else (module Z3_backend.Init( ) : BACKEND)
  (*| "parallel" -> (module Parallel : BACKEND)*)
  | other -> Utils.cmd_option_error "backend" other

let set_encoding () = match Options.sets () with
  | "direct" -> (module DirectSets : SET_ENCODING)
  | "bitvectors" -> (module BitvectorSets : SET_ENCODING)
  | other -> Utils.cmd_option_error "sets" other

let location_encoding () = match Options.locations () with
  | "enum" -> (module DatatypeLocations : LOCATIONS)
  | "bitvectors" -> (module BitvectorLocations : LOCATIONS)
  | other -> Utils.cmd_option_error "location" other

let encoding () =
  let module L = (val location_encoding () : LOCATIONS) in
  let (module H) =
    (module HeapEncoding.Make(L ) : HEAP_ENCODING with type Locations.t = L.t) in
  let (module C) =
    (module Translation_context.Make(L ) :
      CONTEXT with type Locations.t = L.t
               and type HeapEncoding.t = HeapEncoding.Make(L).t
    )
  in
  let module S = (val set_encoding () : SET_ENCODING) in
  let (module Q) = match Options.quantifiers () with
    | "direct" ->
        (module QuantifierEncoding.Direct(L ) :
          QUANTIFIER_ENCODING with type Locations.t = L.t
        )
    | "path" ->
        (module QuantifierEncoding.Path(L ) :
          QUANTIFIER_ENCODING with type Locations.t = L.t
        )
    | "enum" ->
        (module QuantifierEncoding.Enumeration(L ) :
          QUANTIFIER_ENCODING with type Locations.t = L.t
        )
    | "smart_enum" ->
        (module QuantifierEncoding.SmartEnumeration(L ) :
          QUANTIFIER_ENCODING with type Locations.t = L.t
        )
    | other -> Utils.cmd_option_error "quantifier encoding" other
  in
  (module struct
    module Locations = L
    module Context = C
    module SetEncoding = S
    module QuantifierEncoding = Q

    module LS_Encoding = LS_Encodings.Default(C)
    module DLS_Encoding = DLS_Encodings.Default(C)
    module NLS_Encoding = NLS_Encodings.Default(C)
  end : ENCODING)

(** === Parsing === *)

(** Check consistency of options *)
let check () =
  let module Backend = (val backend () : BACKEND) in

  if not @@ Backend.is_available ()
  then failwith "Selected backend solver is not installed";

  if Options.sets () = "bitvectors" && Options.locations () = "enum"
  then failwith "Encoding combining 'bitvector' sets and 'enum' locations is not available";

  if not Backend.supports_sets && Options.sets () = "direct"
  then failwith "Selected backend solver does not support direct set encoding";

  if not Backend.supports_quantifiers && Options.quantifiers () = "direct"
  then failwith "Selected backend solver does not support direct quantifier encoding"

let set_debug () =
  if Options.debug () then
    Options.set_produce_models true
  else ()

let parse () =
  Options.parse ();
  check ();
  set_debug ();
  Options.input_path ()


(*
let location_encoding () = match !locations with
  | "enum" -> (module EnumerationLocations : LOCATIONS)
  | other -> failwith ("unknown location encoding `" ^ other ^ "`")

let heap_encoding locs =
  let module L = (val locs : LOCATIONS) in
  match !heap with
  | "default" ->
      (module HeapEncoding.Make(L)
        : HEAP_ENCODING with type Locations.t = (val locs : LOCATIONS).t)
  | other -> failwith ("unknown heap encoding `" ^ other ^ "`")

let memory_encoding () =
  (module struct
    module Locations = (val location_encoding () : LOCATIONS)
    module HeapEncoding = (val heap_encoding (module Locations) :
      HEAP_ENCODING with type Locations.t = Locations.t)
  end : MEMORY_ENCODING)

let base_encoding () =
  let module M = (val memory_encoding () : MEMORY_ENCODING) in
  Translation_context.Make(M)

let set_encoding () = match !sets with
  | "direct" -> (module DirectSets : SET_ENCODING)
  | "bitvectors" -> (module BitvectorSets : SET_ENCODING)
  | other -> failwith ("unknown set encoding `" ^ other ^ "`")

let quantifier_encoding () =
  let module B = (val base_encoding () : BASE_ENCODING) in
  match !quantifiers with
  | "none" -> QuantifierEncoding.Direct(B)
  | "enum" -> QuantifierEncoding.Enumeration (B)
  | other -> failwith ("unknown quantifier elimination method `" ^ other ^ "`")

let encoding () =
  (module struct
    module B = (val base_encoding () : BASE_ENCODING)
    module SetEncoding = (val set_encoding () : SET_ENCODING)
    module QuantifierEncoding = (val quantifier_encoding () : QUANTIFIER_ENCODING)
    module LS_Encoding = LS_Encodings.Default(BaseEncoding)
    module DLS_Encoding = DLS_Encodings.Default(BaseEncoding)
    module NLS_Encoding = NLS_Encodings.Default(BaseEncoding)
  end)
*)
(*
let _list_encoding = ref "functional"
let list_encoding () = match !_list_encoding with
  (* TODO:
  | "predicate" -> (module ListEncoding.Classic : Translation_sig.LIST_ENCODING)
  | "functional" -> (module ListEncoding.Functional : Translation_sig.LIST_ENCODING)
  (*| "generic" -> (module GenericLists.LS : Translation_sig.LIST_ENCODING)
  *)*)
  | other -> failwith ("unknown list encoding `" ^ other ^ "`")
*)
