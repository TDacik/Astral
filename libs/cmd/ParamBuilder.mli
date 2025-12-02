(* Functors for building parameters.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open Param_sig

exception OptionError of string

module True (P : PARAM) : BOOL

module False (P : PARAM) : BOOL

module Int (P : INT_IN) : INT

module PositiveInt (P : VALUE_IN with type t:= int) : INT

module String (P : VALUE_IN with type t:= string) : STRING

module Path (P : VALUE_IN with type t:= string) : STRING

module Enum (E : ENUM_IN) : ENUM with type t = E.t

module Action (A : ACTION) : ACTION
