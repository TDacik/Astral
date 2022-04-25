(* Length constraints over pairs of variables
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Length = struct

  type t =
    | Infinity        (* x cannot reach y *)
    | Equal           (* x is equal to y and x does not point to y *)
    | EqualCircular   (* x is equal to y and y points to y *)
    | Pointer         (* x points to y *)
    | List            (* list-segment of length at least 2 *)
    [@@deriving compare]

  let show = function
    | Infinity -> "∞"
    | Equal -> "="
    | EqualCircular -> "⟳"
    | Pointer -> "=1"
    | List -> "≥2"

  module Self = struct
    type nonrec t = t
    let compare = compare
    let show = show
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end

(** Powerset domain over lengths *)

module AbstractLength = struct

  include BatSet.Make(Length)

  (** Universal set *)
  let top = of_list [Infinity; Equal; EqualCircular; Pointer; List]

  let is_contradiction = is_empty

  let is_singleton x set = equal set (singleton x)

  let is_subset set xs =
    subset set (of_list xs)
    && not (is_contradiction set)

  (* ==== SSL atomic predicates ==== *)

  let must_eq set =
    is_subset set [Equal; EqualCircular]
    && not @@ is_contradiction set

  let must_neq set =
    is_subset set [Infinity; Pointer; List]
    && not @@ is_contradiction set

  let must_pointer set =
    is_subset set [EqualCircular; Pointer]
    && not (is_contradiction set)

  let must_list set =
    is_subset set [Equal; Pointer; List]
    && not @@ is_contradiction set

  let must_non_empty_list set =
    is_subset set [Pointer; List]
    && not @@ is_contradiction set

  let must_allocated set =
    is_subset set [EqualCircular; Pointer; List]
    && not @@ is_contradiction set

  (* ==== Printers ==== *)

  let show_aux set =
    elements set
    |> List.map Length.show
    |> String.concat ","

  let show ?(shortnames=true) set =
    if not @@ shortnames
    then show_aux set
    else match elements set with
    | [] -> "∅"

    (* Reachable *)
    | [Equal; EqualCircular; Pointer; List] -> "≥0"
    | [Equal; Pointer; List] -> "≥0"
    | [EqualCircular; Pointer; List] -> "≥0"

    (* Reachable in one or more steps *)
    | [Pointer; List] -> "≥1"

    | [Infinity; Pointer; List] -> (*"≠"*)"⊥ ∨ ≥1"

    | _ -> show_aux set


  module Self = struct
    type nonrec t = t
    let compare = compare
    let show = show ~shortnames:false
  end

  include Datatype.Printable(Self)

end
