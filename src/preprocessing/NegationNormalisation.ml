(* Negation normalisation
 *
 * TODO: variadic connectives
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SL

let is_negation phi = match SL.view phi with Not _ -> true | _ -> false
let get_negation phi = match SL.view phi with Not psi -> psi | _ -> assert false

let normalise =
  SL.map_view (function
    | And [f1; f2] -> begin match SL.view f1, SL.view f2 with
      | Not g1, Not g2   -> SL.mk_not @@ SL.mk_or [g1; g2]
      | Not g1, _       -> SL.mk_gneg f2 g1
      | _, Not g2       -> SL.mk_gneg f1 g2
      | _, _           -> SL.mk_and @@ [f1; f2]
    end
    | And psis ->
      let negated, others = List.partition is_negation psis in
      begin match negated with
      | [negation] ->
        let phi = SL.mk_gneg (SL.mk_and others) (get_negation negation) in
        SL.print phi;
        phi
      end
    | Or [f1; f2] -> begin match SL.view f1, SL.view f2 with
      | Not g1, Not g2 -> SL.mk_not @@ SL.mk_and [g1; g2]
      | Not g1, _      -> SL.mk_not @@ SL.mk_gneg g1 f2
      | _, Not g2      -> SL.mk_not @@ SL.mk_gneg g2 f1
      | _, _           -> SL.mk_or [f1; f2]
    end
    | GuardedNeg (f1, f2) -> begin match SL.view f1, SL.view f2 with
      | Not g1, Not g2 -> SL.mk_gneg g2 g1
      | Not g1, _      -> SL.mk_not @@ SL.mk_or [g1; f2]
      | _, Not g2      -> SL.mk_and [f1; g2]
      | _, _           -> SL.mk_gneg f1 f2
    end
    | Not phi -> begin match SL.view phi with
      | Not psi -> psi (* Double negation elimination *)
      | _ -> SL.mk_not phi
    end
  )

(** Perform normalisation until fixpoint is reached
    TODO: single pass?
 *)
let rec apply phi =
  let phi' = normalise phi in
  if equal phi' phi then phi'
  else apply phi'
