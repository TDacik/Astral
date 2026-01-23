(* Unicode symbols for pretty printing.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let use_unicode = ref true
let use_easter_eggs = ref false

let eq   = ref "="
let neq  = ref "neq"

let and_ = ref "and"
let or_  = ref "or"
let not  = ref "not"

let exists = ref "exists"
let forall = ref "forall"

let entails = ref "|="

let bottom = ref "_|_"

let empty_set = ref "empty"

let maps_to = ref "|->"
let star = ref "*"
let septraction = ref "-(*)"

let defined = ref ":="

let lower_index n = failwith "TODO"

let alien = ref ""

let set_ref (r : string ref) value =
  if !use_unicode then r := value

let set_egg (r : string ref) value =
  if !use_easter_eggs then r := value

let init unicode =
  use_unicode := unicode;
  set_ref eq "=";
  set_ref neq "≠";
  set_ref and_ "∧";
  set_ref or_ "∨";
  set_ref not "¬";
  set_ref exists "∃";
  set_ref forall "∀";
  set_ref entails "⊧";
  set_ref maps_to "↦";
  set_ref star "✲";
  set_ref septraction "-⍟";
  set_ref empty_set "∅";
  set_ref bottom "⊥";
  set_ref defined "≜"

let easter_eggs value =
  use_easter_eggs := value;
  set_egg alien "👽"
