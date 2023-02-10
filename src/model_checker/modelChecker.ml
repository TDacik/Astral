(* Model validation for positive formulae.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021

open SSL

module SH = StackHeapModel
open SH

module Print = Printer.Make(struct let name = "Model checker" end)

exception IncorrectFootprint of SSL.t

(** Compute footprints according the unique footprint property *)
let rec compute_footprint (sh : SH.t) phi =
  match SSL.get_arity phi with
  | Atom _ -> begin match phi with
    | Eq _ | Neq _ -> Footprint.empty
    | PointsTo (x, _) -> Footprint.singleton (Stack.find x sh.stack)
    | LS (x, y) ->
        let sx = Stack.find x sh.stack in
        let sy = Stack.find y sh.stack in
        if Location.equal sx sy then Footprint.empty
        else
          try SH.get_path sh sx sy
              |> Footprint.of_list
          with _ -> Footprint.empty (* TODO: should be handled in SH *)
    end
  | Binary (psi1, psi2) ->
      let fp1 = compute_footprint sh psi1 in
      let fp2 = compute_footprint sh psi2 in
      begin match phi with
      | And _ -> fp1
      | GuardedNeg _ -> fp1
      | Or _ -> SH.get_footprint sh phi   (* This has to be guessed *)
      | Star _ -> Footprint.union fp1 fp2
      | Septraction _ ->
          let sh' = (SH.get_subformula_model sh phi) in
          let fp1 = compute_footprint sh' psi1 in
          let fp2 = compute_footprint sh' psi2 in
          Footprint.diff fp2 fp1
      end

(** Check whether model satisfies formula, ignoring footprints *)
let rec check_sat sh phi =
  let stack = SH.get_stack sh in
  let heap = SH.get_heap sh in
  let heap_graph = SH.get_heap_graph sh in
  let check = check_sat sh in
  match phi with
  | And (psi1, psi2) -> check psi1 && check psi2
  | GuardedNeg (psi1, psi2) ->
      check psi1 &&
      (not @@ check psi2
       || not @@ Footprint.equal (SH.get_footprint sh psi1) (SH.get_footprint sh psi2)
      )
  | Or (psi1, psi2) ->
      let fp = compute_footprint sh phi in
      let fp1 = compute_footprint sh psi1 in
      let fp2 = compute_footprint sh psi2 in
      (check psi1 && Footprint.equal fp fp1)
      || (check psi2 && Footprint.equal fp fp2)
  | Star (psi1, psi2) ->
    let fp1 = get_footprint sh psi1 in
    let fp2 = get_footprint sh psi2 in
    check psi1 && check psi2 && Footprint.disjoint fp1 fp2

  | Eq (x, y) -> Stack.find x stack = Stack.find y stack
  | Neq (x, y) -> Stack.find x stack <> Stack.find y stack
  | PointsTo (x, y) -> begin
      try Heap.find (Stack.find x stack) heap = (Stack.find y stack)
      with Not_found -> false
    end
  | LS (x, y) ->
      let x = SH.Stack.find x sh.stack in
      let y = SH.Stack.find y sh.stack in
      HeapGraph.has_path heap_graph x y

  | Septraction (psi1, psi2) ->
      let sh' = (SH.get_subformula_model sh phi) in
      let fp1 = get_footprint sh' psi1 in
      let fp2 = get_footprint sh' psi2 in
      Printf.printf "%s" (SH.to_string sh');
      check_sat sh' psi1
      && check_sat sh' psi2
      && Footprint.subset fp1 fp2
      (** TODO: && heap equality *)

(** Top-level function *)
let check sh phi =
  let fp = compute_footprint sh phi in
  check_sat sh phi && Footprint.equal fp (SH.get_footprint sh phi)

*)
