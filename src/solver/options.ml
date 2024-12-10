(* High-level access to command-line options.
 *
 * This module needs to be factored out of Options_base to prevent circular dependencies.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Backend_sig
open Location_sig
open Encoding_context_sig
open SetEncoding_sig
open HeapEncoding_sig
open Translation_sig

include Options_base
module Options = Options_base

let set_produce_models flag =
  Options._produce_models := flag

type backend = [`Bitwuzla | `CVC5 | `Z3 | `Auto ]
type encoding = [`Bitvectors | `Sets]

let sets_encoding () = match Options.sets () with
  | "direct" -> (module DirectSets : SET_ENCODING)
  | "bitvectors" -> (module BitvectorSets : SET_ENCODING)
  | other -> Utils.cmd_option_error "sets" other

let location_encoding () = match Options.locations () with
  | "enum" -> (module DatatypeLocations : LOCATIONS)
  | "bitvectors" -> (module BitvectorLocations : LOCATIONS)
  | other -> Utils.cmd_option_error "location encoding" other

let auto_selection_of_backend () =
  if not @@ Options.produce_models ()
     && Options.sets () = "bitvectors"
     && Bitwuzla_backend.is_available ()
  then
    (module Bitwuzla_backend : BACKEND)
  else if CVC5_backend.is_available () then
    (module CVC5_backend : BACKEND)
  else
    (module Z3_backend.Init( ) : BACKEND)

let backend () = match Options.backend () with
  | "bitwuzla" -> (module Bitwuzla_backend : BACKEND)
  | "boolector" -> (module Boolector_backend : BACKEND)
  | "cvc5" -> (module CVC5_backend : BACKEND)
  | "z3" -> (module Z3_backend.Init( ) : BACKEND)
  | "yices2" -> (module Yices_backend : BACKEND)
  | "auto" -> auto_selection_of_backend ()
  (*| "parallel" -> (module Parallel : BACKEND)*)
  | other -> Utils.cmd_option_error "backend" other

let encoding () =
  let module L = (val location_encoding () : LOCATIONS) in

  let (module H) =
    (module ArrayEncoding.Make(L) : HEAP_ENCODING with type Locations.internal = L.internal)
  in

  let (module C) =
    (module Translation_context.Make(L)(H) : ENCODING_CONTEXT
       with type Locations.internal = L.internal
        and type HeapEncoding.t = H.t
        and type HeapEncoding.Locations.internal = L.internal
        and type t = (L.t, H.t) Encoding_context_sig.t
    )
  in

  let module S = (val sets_encoding () : SET_ENCODING) in
  let (module Q) = match Options.quantifiers () with
    | "direct" ->
        (module QuantifierEncoding.Direct(L) :
          QUANTIFIER_ENCODING with type Locations.internal = L.internal
        )
    | "path" ->
        (module QuantifierEncoding.Path(L) :
          QUANTIFIER_ENCODING with type Locations.internal = L.internal
        )
    | "enum" ->
        (module QuantifierEncoding.Enumeration(L) :
          QUANTIFIER_ENCODING with type Locations.internal = L.internal
        )
    | "smart_enum" ->
        (module QuantifierEncoding.SmartEnumeration(L) :
          QUANTIFIER_ENCODING with type Locations.internal = L.internal
        )
    | other -> Utils.cmd_option_error "quantifier encoding" other
  in
  (module struct
    module Locations = L
    module HeapEncoding = H
    module Context = C
    module SetEncoding = S
    module QuantifierEncoding = Q

  end : ENCODING)

(** === Setters === *)

let set_backend = function
  | `Bitwuzla -> Options_base.set_backend "bitwuzla"
  | `CVC5 -> Options_base.set_backend "cvc5"
  | `Z3 -> Options_base.set_backend "z3"
  | `Auto ->
    if Bitwuzla_backend.is_available () then Options_base.set_backend "bitwuzla"
    else Options_base.set_backend "z3"

let set_encoding = function
  | `Sets -> Options_base.set_encoding "enum"
  | `Bitvectors -> Options_base.set_encoding "bitvectors"

(** === Parsing === *)

(** Check consistency of options *)
let check () =
  let module Backend = (val backend () : BACKEND) in

  if not @@ Backend.is_available ()
  then failwith "Selected backend solver is not installed";

  if Options.sets () = "bitvectors" && Options.locations () = "enum"
  then failwith "Encoding combining 'bitvector' sets and 'enum' locations is not available";

  if not Backend.supports_sets && Options.sets () = "direct"
  then failwith @@
    Format.asprintf "Selected backend solver (%s) does not support direct set encoding"
      Backend.name
  ;

  if Options.produce_models () && not @@ Backend.supports_get_info then begin
    Utils.warning "Selected backend does not support producing of models";
    exit 1
  end;

  if not Backend.supports_quantifiers && Options.quantifiers () = "direct"
  then failwith @@
    Format.asprintf "Selected backend solver (%s) does not support direct quantifier encoding"
      Backend.name

let _set_debug () =
  if Options.debug () then
    Options.set_produce_models true
  else ()


let parse () =
  Options.parse ();
  check ();
  _set_debug ();
  UnicodeSymbols.easter_eggs (Options.easter_eggs () || Options.debug ());
  UnicodeSymbols.init @@ Options.unicode ();
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
