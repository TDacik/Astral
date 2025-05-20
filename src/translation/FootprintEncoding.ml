open SMT
open ThreeValuedLogic
open Translation_context

module Logger = Logger.MakeWithDir(struct
  let name = "FootprintConstruction"
  let level = 2
  let dirname = "footprints"
end)

module Make (Encoding : Translation_sig.ENCODING) (Backend : Backend_sig.BACKEND) = struct

  module Translation = Translation.Make(Encoding)(Backend)

  module Cache = struct
    include Stdlib.Map.Make(struct
      type t = SL.Term.t * SL.Term.t List.t [@@deriving compare]
    end)

    let self = ref empty

    let reset () = self := empty

    let add key data = self := add key data !self

    let mem key = mem key !self

    let find key = find key !self

  end

  let check cond = match Backend.check_sat cond with
    | SMT_Unsat _ -> False
    | _ ->
      begin match Backend.check_sat @@ SMT.Boolean.mk_not cond with
        | SMT_Unsat _ -> True
        | _ -> Unknown
      end

  let rec mk_footprint_boundaries ~allocated n (ctx : (_, _) t) abs root (boundaries : SL.Term.t list) =
    Logger.debug "Footprint %d: %a \n" n SL.Term.pp root;
    if n = 0 then Sets.mk_empty ctx.fp_sort
    else if Cache.mem (root, boundaries) then Cache.find (root, boundaries)
    else if (ThreeValuedLogic.is_true (
        let cond =
          Boolean.mk_or @@ List.map (fun x -> SMT.mk_eq [x; Translation.translate_term ctx root]) allocated
        in
        let res = check cond in
        Logger.debug "Checking back %s -> %s\n" (SMT.show cond) (ThreeValuedLogic.show res);
        res
      )) then Sets.mk_empty ctx.fp_sort
    else
      let _ = assert (n > 0) in
      let root_t = Translation.translate_term ctx root in
      let holes = List.map (Translation.translate_term ctx) boundaries in
      let empty_guard = Boolean.mk_or @@ List.map (fun hole -> SMT.mk_eq [root_t; hole]) (holes @ allocated) in

      let alloc = Sets.mk_singleton root_t in

      (* For each field of a structure pointed-to by the sort of root, recursively create its footprint *)
      let mk_recursive () =
        Sets.mk_union ctx.fp_sort @@ List.map (fun field ->
          let select_x = SL.Term.mk_heap_term field root in
          let allocated' = root_t :: allocated in
          mk_footprint_boundaries (n-1) ~allocated:allocated' ctx abs select_x boundaries
        ) abs.PredicateAbstraction.skeleton_fields
      in

      let if_then = Sets.mk_empty ctx.fp_sort in
      let if_else () =
        Backend.push (Boolean.mk_not empty_guard);
        let res = Sets.mk_union ctx.fp_sort [mk_recursive (); alloc] in
        Backend.pop 1;
        res
      in
      let res_aux = check empty_guard in
      let res = match res_aux with
        | True -> if_then
        | False -> if_else ()
        | _ -> Boolean.mk_ite empty_guard if_then (if_else ())
      in
      Logger.debug "Checking %s -> %s\n" (SMT.show empty_guard) (ThreeValuedLogic.show res_aux);
      Cache.add (root, boundaries) res;
      res

  (** Create a unique footprint of a predicate pred. *)
  let mk_unique_footprint ~allocated (ctx : (_, _) t) bound abs (id : InductiveDefinition.t) xs =
    Logger.debug "%s(%s)\n" (id.name) (SL.Term.show_list xs);

    let root = PredicateAbstraction.get_root abs ~params:xs in
    let holes = BatList.unique_cmp ~cmp:SL.Term.compare
                @@ SL.Term.nil :: PredicateAbstraction.get_holes abs ~params:xs in

    let res = mk_footprint_boundaries ~allocated bound ctx abs root holes in
    (*SL.print ~prefix:"Instance: " @@ SL.mk_predicate id.name xs;
    SL.Term.print_list ~prefix:"Holes: " holes;
    SMT.print_list ~prefix:"Res: " res;*)
    [res]

  (** For all may-allocated variable, we consider each possibility wether it is allocated
      or nor. *)
  let mk_non_unique_footprint ~allocated ctx bound abs id params =
    let root = PredicateAbstraction.get_root abs ~params in
    let may_allocated = PredicateAbstraction.may_allocated abs ~params in
    let worklist = List_utils.sublists may_allocated in
    List.map (fun boundaries -> mk_footprint_boundaries ~allocated bound ctx abs root boundaries) worklist

  let has_unique_footprint abs =
    let may_allocated = PredicateAbstraction.may_allocated abs in
    List.is_empty may_allocated

  (** Create a list of terms representing footprints of an inductive predicate. *)
  let mk_footprint (ctx : (_, _) t) ~allocated (lhs : SMT.t) id xs =
    let open InductiveDefinition in
    let abs = SID.abstraction id.name in
    let bound = LocationBounds.sum ctx.location_bounds - 1 in (* Minus one for nil *)
    let allocated = List.map (Translation.translate_term ctx) allocated in
    if has_unique_footprint abs
    then mk_unique_footprint ~allocated ctx bound abs id  xs
    else mk_non_unique_footprint ~allocated ctx bound abs id xs

end
