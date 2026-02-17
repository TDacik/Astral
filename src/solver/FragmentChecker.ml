(* Check whether formula lies inside supported fragment.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Context
open Result_syntax
open InductiveDefinition

module Logger = Logger.Make(struct let name = "FragmentChecker" let level = 2 end)

let check_low_level_sl ctx =
  if SL.is_low_level ctx.phi && not @@ HeapSort.is_bitvector_model ctx.raw_input.heap_sort then
    Result.error
      "Low-level SL (with begin/end operations) defined over sort different than bitvectors"
  else Result.ok ()

(** Checks for individual inductive definitions *)

let check_progress name psi =
  let n = List.length @@ SL.select_subformulae SL.is_pointer psi in
  if n = 0 && not @@ Inlining.can_be_inlined name then (* TODO: use cache *)
    Result.error @@ Format.asprintf "Predicate %s: case %s does not satisfy progress property" name (SL.show psi)
  else if n > 1 then Result.error @@ Format.asprintf "Predicate %s: case %s has more than 1 points-to assertion" name (SL.show psi)
  else Result.ok ()

let check_connectivity psi =
  let pto = List.hd @@ SL.select_subformulae SL.is_pointer psi in
  let ys = match SL.view pto with PointsTo (_, _, ys) -> ys in
  let calls = SL.select_subformulae SL.is_predicate psi in
  List.for_all (fun call ->
    let root = SL.get_root call in
    SL.Term.MonoList.mem root ys (* TODO: propagate eqs *)
  ) calls

let check_id_case name psi =
  (*if not @@ SL.is_symbolic_heap psi then Result.error @@
    Format.asprintf "Predicate %s: case %s is not a symbolic heap" name (SL.show psi)
  else*)
  if not @@ check_connectivity psi then Result.error @@
    Format.asprintf "Predicate %s: case %s is not connected" name (SL.show psi)
  else
    check_progress name psi

let check_id id =
  List.fold_left (fun acc case ->
    Result.bind acc @@ (fun _ -> check_id_case id.name case)
  ) (Result.ok ()) id.inductive_cases

let check_inductive_definitions () =
  GlobalSID.fold_user_defined (fun id acc ->
    Result.bind acc @@ (fun _ -> check_id id)
  ) (Result.ok ())

let check_formula ctx =
  if SLID.has_user_defined_predicates ctx.phi then
    match SL.classify_fragment ctx.phi with
      | Atomic | SL.SymbolicHeap_SAT | SL.SymbolicHeap_ENTL -> Result.Ok ()
      | _ -> Result.error "User-defined inductive predicates supported only in the symbolic heap fragment"
  else Result.Ok ()

let check ctx =
  let* res1 = check_low_level_sl ctx in
  let* res2 = check_inductive_definitions () in
  let* res3 = check_formula ctx in
  check_inductive_definitions ()
