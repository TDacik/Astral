(* Check whether formula lies inside supported fragment.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Context
open Result_syntax
open InductivePredicate

let check_low_level_sl ctx =
  if SL.is_low_level ctx.phi && not @@ HeapSort.is_bitvector_model ctx.raw_input.heap_sort then
    Result.error
      "Low-level SL (with begin/end operations) defined over sort different than bitvectors"
  else Result.ok ()

(** Checks for inductive definitions *)

let rec check_progress psi = SL.is_atomic psi || (match SL.view psi with
    | Exists (_, psi) -> check_progress psi
    | Star psis -> List.exists check_progress psis
    | PointsTo _ -> true
    | Ite (_, then_, else_) -> List.for_all check_progress [then_; else_]
    | _ -> false
  )

let check_id_case name psi =
  (*if not @@ SL.is_symbolic_heap psi then Result.error @@
    Format.asprintf "Predicate %s: case %s is not a symbolic heap" name (SL.show psi)
  else*) if not @@ check_progress psi then Result.error @@
    Format.asprintf "Predicate %s: case %s does not satisfy progress property" name (SL.show psi)
  else Result.ok ()

let check_id id =
  List.fold_left (fun acc case ->
    Result.bind acc @@ (fun _ -> check_id_case id.name case)
  ) (Result.ok ()) id.inductive_cases

let check_inductive_definitions () =
  SID.fold_on_user_defined (fun id acc ->
    Result.bind acc @@ (fun _ -> check_id id)
  ) (Result.ok ())

let check ctx =
  let* res1 = check_low_level_sl ctx in
  check_inductive_definitions ()
