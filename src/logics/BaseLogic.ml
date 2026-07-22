include BaseLogic_config

include BaseLogic_terms
include BaseLogic_DOT
include BaseLogic_sexp

(* TODO: extract just type error *)
include BaseLogic_utils


(* Theories *)
module Boolean = BaseLogic_boolean.Smart
module Equality = BaseLogic_equality.Smart
module Arithmetic = BaseLogic_arithmetic.Smart
module Bitvector = BaseLogic_bitvectors.Bitvector
module Sets = BaseLogic_sets.Sets
module Array = BaseLogic_arrays.Array
module Enumeration = BaseLogic_enumeration.Enumeration

module Quantifiers = BaseLogic_quantifiers.Quantifiers

module SeparationLogic = BaseLogic_seplog.SeparationLogic
