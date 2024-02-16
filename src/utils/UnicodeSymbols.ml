(* Unicode symbols for pretty printing.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let select (unicode, plain) =
  if (*Options.unicode ()*) true then
    (fun () -> unicode)
  else
    (fun () -> plain)

let eq  = select ("=", "=")
let neq = select ("≠", "neq")

let logand = select ("∧", "and")
let logor  = select ("∨", "or")
let lognot = select ("¬", "not")
let gneg   = select ("∧¬", "and not")

let exists = select ("∃", "exists")
let forall = select ("∀", "forall")

let entails = select ("⊧", "|=")

let pto =  select ("↦", "pto")
let star = select ("∗", "star")
let septraction = select ("-⍟", "septraction")
