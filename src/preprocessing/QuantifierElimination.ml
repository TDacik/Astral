(* Elimination of quantifiers in SL formulae.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SL

module Logger = Logger.MakeWithDir (struct
  let name = "Quantifier elimination"
  let level = 2
  let dirname = "qelim"
end)

(** Run skolemisation and add skolem variables into model adapter. *)
let skolemisation ctx =
  let open Context in
  let phi', skolems = SL.skolemisation ctx.phi in
  let ctx' = {ctx with phi = phi'} in
  List.fold_left (Context.add_skolem_var) ctx' skolems

(** Remove binders not contained in quantifier bodies. *)
let remove_useless phi =
  let filter_fn psi x =
    (* TODO: check loc vars using heap sort *)
    let res = BatList.mem_cmp SL.Variable.compare x (SL.free_vars ~with_pure:true psi) in
    if not res then Logger.debug "Removing unused variable %s\n" (SL.Variable.show x) else ();
    res
  in
  SL.map_view (function
    | Exists (xs, psi) -> `Modify (SL.mk_exists (List.filter (filter_fn psi) xs) psi)
    | Forall (xs, psi) -> `Modify (SL.mk_forall (List.filter (filter_fn psi) xs) psi)
    | _ -> `Skip
  ) phi

module Instance = struct

  type t = SL.Term.t Option.t [@@deriving compare]

  let show = function
    | None -> "nothing"
    | Some t -> SL.Term.show t

  let join_list xs : t = match List.filter Option.is_some xs with
    | [] -> None
    | xs -> List.hd xs

  include Datatype.Printable(struct
    type nonrec t = t
    let show = show
  end)

  let rec compute_determined_value x (ground : SL.Variable.t list) psi =
    let continue = compute_determined_value x ground in
    match SL.view psi with
      | PointsTo (s, def, ys) ->
        let open MemoryModel.StructDef in
        let vars = SL.Term.free_vars s in
        if SL.Variable.Set.subset (SL.Variable.Set.of_list vars) (SL.Variable.Set.of_list ground) then
          let index = List.find_index (fun t -> SL.Term.equal t @@ SL.Term.of_var x) ys in
          Option.map (fun i -> SL.Term.mk_heap_term (List.nth def.fields i) s) index
        else None
      | Eq es ->
        if BatList.mem_cmp SL.Term.compare (SL.Term.of_var x) es then
          let global = List.filter (fun e -> SL.Term.is_ground ~ground e) es in
          match global with [] -> None | g :: _ -> Some g (* TODO: why just g? *)
        else None
      | Distinct _ | Predicate _ | Emp -> None
      | Star psis | And psis -> join_list @@ List.map continue psis
      | Or psis -> None
      | GuardedNeg (lhs, _) -> continue lhs
      | Ite (c, t, e) ->
        (* If condition c is build of only ground terms, we can use it in the instance *)
        if SL.Variable.Set.subset (SL.Variable.Set.of_list @@ SL.get_vars c) (SL.Variable.Set.of_list ground) then
          let eq_xs = match SL.view c with Eq xs -> xs | _ -> failwith (SL.show c) in (* TODO: Change Ite -> IfEqual? *)
          let t_res = continue t in
          let e_res = continue e in
          begin match t_res, e_res with
          | Some t, Some e -> Option.some @@ SL.Term.mk_if_equal eq_xs t e
          | _, _ -> None
        end
        else None
      | Exists (xs, psi) -> continue psi
      | _ -> failwith @@ SL.show psi

end

let remove_binder sl_graph phi psi (x : SL.Variable.t) =
  let _ = Logger.debug "Eliminating quantifier var %s\n" (SL.Variable.show x) in
  let vals = Instance.compute_determined_value x (SL.free_vars ~with_pure:true phi) psi in (* TODO *)
  match vals with
    | Some v ->
      let _ = Logger.debug "Eliminated %s using substitution: %s\n" (SL.Variable.show x) (SL.Term.show v) in
      SL.substitute psi ~var:x ~by:v, []
    | None ->
      let _ = Logger.debug "Cannot eliminate quantifier var: %s\n" (SL.Variable.show x) in
      psi, [x]

(** Remove quantifed variables with determined values *)
let remove_determined sl_graph phi =
  SL.map_view (function
    | Exists (vars, psi) ->
      let psi, xs = List.fold_left (fun (psi, xs) x ->
        let psi', xs' = remove_binder sl_graph phi psi x in
        psi', xs @ xs'
      ) (psi, []) vars
      in
      `Modify (SL.mk_exists xs psi)
    | _ -> `Skip
  ) phi

let apply sl_graph phi =
  if SL.is_quantifier_free phi then phi
  else
    remove_useless phi
    |> RemoveVariadic.apply ~symbolic_heap:true (* TODO: is removal needed? *)
    |> remove_determined sl_graph

let apply_ctx ctx =
  let open Context in
  skolemisation ctx
  |> (fun ctx -> {ctx with phi = apply ctx.sl_graph ctx.phi})
