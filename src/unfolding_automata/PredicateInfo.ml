(* Collection of information computed for inductive definitions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module Signature = struct
  include Set.Make(Sort.Set)

  let flatten s = fold Sort.Set.union s Sort.Set.empty

end

module Entry = struct

  type t = {
    root : SL.Variable.t;
    allocated : SL.Variable.t list;       (** Variables allocated in all non-empty models *)
    never_allocated : SL.Variable.t list; (** Variables not allocated in all models *)

    signature : Signature.t;

    (*counting_abstraction : CountingAbstraction.t;*)

    stable_depth : UnfoldingBound.t;
    (** Number of steps needed to stabilize pure constraints. *)

    unfolding_depth : UnfoldingBound.t;
    (** Number of steps needed for the unfolding procedure. *)
  }

  let show self =
    Format.asprintf "@[<hov 2>@.  - root: %s@.  - allocated: %s@.  - never allocated: %s@.  \
                    - signature: %s@.
                    - stable depth: %s@.  - unfolding depth: %s@ @]"
      (SL.Variable.show self.root)
      (SL.Variable.show_list self.allocated)
      (SL.Variable.show_list self.never_allocated)
      (String.concat " | "
        @@ List.map (fun case -> "[" ^ Sort.Set.show case ^ "]")
        @@ Signature.elements self.signature)
      (UnfoldingBound.show self.stable_depth)
      (UnfoldingBound.show self.unfolding_depth)


end

include InductiveDefinition.MonoMap(Entry)
