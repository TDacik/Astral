open ID_sig

type t =
  | Builtin of (module BUILTIN)
  | UserDefined of InductivePredicate.t

let name = function
  | Builtin (module B : BUILTIN) -> B.name
  | UserDefined id -> InductivePredicate.show id

let show pred =
  let kind = match pred with
    | Builtin _ -> "built-in"
    | UserDefined _ -> "user defined"
  in
  Format.asprintf "%s (%s)" (name pred) kind
