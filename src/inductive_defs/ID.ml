open ID_sig

type t =
  | Builtin of (module BUILTIN)
  | UserDefined of InductiveDefinition.t

let name = function
  | Builtin (module B : BUILTIN) -> B.name
  | UserDefined id -> InductiveDefinition.show id

let show pred =
  let kind = match pred with
    | Builtin _ -> "built-in"
    | UserDefined _ -> "user defined"
  in
  Format.asprintf "%s (%s)" (name pred) kind

let to_id = function
  | Builtin (module B : BUILTIN) ->
    let xs = List.map (fun sort -> SL.Variable.mk_fresh "x" sort) B.signature in
    InductiveDefinition.mk B.name xs @@ SL.mk_or @@ B.rules (xs, B.default_instantiation)
  | UserDefined id -> id
