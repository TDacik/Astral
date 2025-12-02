(* Abstraction of predicates derived from small-model computation.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

type t = {
  id : InductiveDefinition.t;
  root : SL.Variable.t;
  allocated : SL.Variable.t list;       (** Variables allocated in all non-empty models *)
  never_allocated : SL.Variable.t list; (** Variables that are never allocated. *)

  skeleton_fields: MemoryModel.Field.t list;

  stable_size : int;
  fixpoint_size : float;
  unfolding_depth : int;
}

let param_position self var =
  fst @@ BatList.findi (fun _ -> SL.Variable.equal var) self.id.header

let get_root ?params self = match params with
  | None -> SL.Term.of_var self.root
  | Some xs -> List.nth xs (param_position self self.root)

let get_holes ?params self =
  let hole_params =
    self.id.header
    |> List.filter (fun v -> not @@ BatList.mem_cmp SL.Variable.compare v self.allocated)
  in
  match params with
  | None -> List.map SL.Term.of_var hole_params
  | Some xs -> List.map (fun h -> List.nth xs (param_position self h)) hole_params

let get_must_allocated ?params self =
  match params with
  | None -> List.map SL.Term.of_var self.allocated
  | Some xs -> List.map (fun h -> List.nth xs (param_position self h)) self.allocated

let get_may_dangling ?params self =
  let allocated = get_must_allocated ?params self in
  let args = Option.value params ~default:(List.map SL.Term.of_var self.id.header) in
  List.filter (fun t -> not @@ SL.Term.MonoList.mem t allocated) args

let may_allocated ?params self =
  let must = self.allocated @ self.never_allocated in
  let formals = List.filter (fun v -> not @@ BatList.mem_cmp SL.Variable.compare v must) self.id.header in
  match params with
  | None -> List.map SL.Term.of_var formals
  | Some xs -> List.map (fun h -> List.nth xs (param_position self h)) formals

let show self : string =
  Format.asprintf "@[<hov 2>@.  Root: %s@.\
                   Skeleton: %s@.\
                   Allocated %s@.\
                   Never allocated %s@.\
                   May dangling %s@.
                   Stable-size: %d@.\
                   Fixpoint-size: %f@.\
                   Unfolding depth:%d@ @]"
    (SL.Variable.show self.root)
    (MemoryModel.Field.show_list self.skeleton_fields)
    (SL.Variable.show_list self.allocated)
    (SL.Variable.show_list self.never_allocated)
    (SL.Term.show_list @@ get_may_dangling self)
    self.stable_size
    self.fixpoint_size
    self.unfolding_depth

module M = struct
  include InductiveDefinition.MonoMap(struct
    type nonrec t = t
    let show = show
  end)
end
