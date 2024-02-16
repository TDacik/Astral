(* Predicate bounds.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Field

module Entry = struct

  type nested_bounds = {
    concrete : Interval.t list;
    default : Interval.t;
  }

  type t =
    | LS_bound of Interval.t
    | DLS_bound of Interval.t * Interval.t
    | NLS_bound of Interval.t * nested_bounds

  let default (bounds : LocationBounds.t) pred =
    SSL.get_root pred
    |> Variable.get_sort
    |> (fun s -> LocationBounds.total s bounds)
    |> (fun m ->
        match pred with
          | LS _ -> LS_bound (0, m)
          | DLS _ -> DLS_bound ((0, m), (0, m))
          | NLS (_, _, z) ->
              let top = (0, m) in
              let next = LocationBounds.total Sort.loc_ls bounds in
              NLS_bound (top, {concrete = []; default = (0, next)})
       )

  let map f = function
    | LS_bound n -> LS_bound (f n)
    | DLS_bound (n, p) -> DLS_bound (f n, f p)
    | NLS_bound (n, {concrete = cs; default = d}) ->
      NLS_bound (f n, {concrete = List.map f cs; default = f d})

  let join e1 e2 = match e1, e2 with
    | LS_bound i1, LS_bound i2 -> LS_bound (Interval.join i1 i2)
    | DLS_bound (ni1, pi1), DLS_bound (ni2, pi2) ->
      DLS_bound (Interval.join ni1 ni2, Interval.join pi1 pi2)
    | _, _ -> e1 (* TODO *)

  let mk_ls bound = LS_bound bound

  let mk_dls next prev = DLS_bound (next, prev)

  let mk_nls top concrete default = NLS_bound (top, {concrete; default})

  let show ?(indent="  ") = function
    | LS_bound bound -> Interval.show bound
    | DLS_bound (next, prev) ->
      Format.asprintf "%sforward: %s\n%sbackward: %s"
        indent (Interval.show next)
        indent (Interval.show prev)
    | NLS_bound (top, nested) ->
      Format.asprintf "%stop: %s\n%s%s\n%sdefault: %s"
        indent (Interval.show top)
        indent (String.concat ("\n" ^ indent ^ indent) @@ List.map Interval.show nested.concrete)
        indent (Interval.show nested.default)

end

module M = SSL.Map

include M

let show = M.show Entry.show

let add key entry bounds =
  if M.mem key bounds then
    let old_entry = M.find key bounds in
    let new_bound = Entry.join old_entry entry in
    M.add key new_bound bounds
  else M.add key entry bounds

let join bounds1 bounds2 =
  let join_fn _ bound1 bound2 = Some (Interval.join bound1 bound2) in
  M.union join_fn bounds1 bounds2

and ls_bound g phi psi x y loc_bounds =
  let max = try LocationBounds.total Sort.loc_ls loc_bounds with _ -> 0 in
  let bound = PathBound.compute g Next x y max in
  Entry.mk_ls bound

(** Bound of doubly-linked list is the join of path_bound going forward (next)
    and path_bound going forward (prev). *)
and dls_bound g phi psi x y _ _ loc_bounds =
  let max = LocationBounds.total Sort.loc_dls loc_bounds in
  let next_bound = PathBound.compute g Next x y max in
  let prev_bound = PathBound.compute g Prev y x max in
  Entry.mk_dls next_bound prev_bound

and nls_bound g phi psi x y z loc_bounds =
  let ls_bound =
    try LocationBounds.total Sort.loc_ls loc_bounds + 1
    with Not_found -> 1
  in
  let max = LocationBounds.total Sort.loc_nls loc_bounds in
  let partial_path = PathBound.partial_path g Top x y in
  let top_bound = PathBound.compute g Top x y max in

  let concrete, used =
    List.fold_right
      (fun loc (acc, used) ->
        try
          let bound = PathBound.compute g Next loc z ls_bound in
          let bound = Interval.max 1 bound in
          let used = used + (fst bound) - 1 in
          bound :: acc, used

        with Not_found -> (acc, used)
      ) partial_path ([], 0)
  in
  let default = Interval.max 1 (0, ls_bound - used) in
  Entry.mk_nls top_bound concrete default


let compute_pred phi g loc_bounds psi =
  match Options_base.predicate_bounds () with
    | `None -> Entry.default loc_bounds psi
    | `Upper ->
      let entry = match psi with
        | LS (Var x, Var y) -> ls_bound g phi psi x y loc_bounds
        | DLS (Var x, Var y, Var px, Var ny) -> dls_bound g phi psi x y px ny loc_bounds
        | NLS (Var x, Var y, Var z) -> nls_bound g phi psi x y z loc_bounds
      in
      Entry.map (fun (_, max) -> (0, max)) entry

    | `Both -> match psi with
      | LS (Var x, Var y) -> ls_bound g phi psi x y loc_bounds
      | DLS (Var x, Var y, Var px, Var ny) -> dls_bound g phi psi x y px ny loc_bounds
      | NLS (Var x, Var y, Var z) -> nls_bound g phi psi x y z loc_bounds

let rec compute g loc_bounds toplevel_phi phi = match SSL.node_type phi with
  | Var _ -> []
  | Quantifier (_, term) -> compute g loc_bounds toplevel_phi term
  | Operator (terms, _) | Connective terms -> match phi with
    | psi when SSL.is_predicate psi -> [(psi, compute_pred toplevel_phi g loc_bounds psi)]
    | _ -> List.concat @@ List.map (compute g loc_bounds toplevel_phi) terms

let compute g loc_bounds phi =
  compute g loc_bounds phi phi
  |> List.fold_left
      (fun acc (pred, entry) ->
        add pred entry acc
      ) M.empty
