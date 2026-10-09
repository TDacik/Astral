(* Elimination of heap terms.
 *
 * We expect the formula to be self-framed, i.e., whenever we have a heap term of the
 * form field[x], x must be surely allocated. Therefore, there must exists a point-to
 * atom x |-> <field: y, ...> and we can substitute field[x] with y.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

open MemoryModel

module Logger = Logger.Make (struct let name = "Heap-term elim" let level = 3 end)

(** Exception raised when the input is not self-framed *)
exception NotSelfFramed of SL.Term.t

(*
let find_equivalent g term =
  let candidates = SL_graph.fold_vertex (fun x acc ->
    if SL_graph.must_eq g term x && SL.Term.is_sl_var x && (not @@ SL.Term.equal x term) then x :: acc
    else acc
  ) g []
  in
  let candidates = List.filter (fun t -> not @@ String.contains (SL.Term.show t) '!') candidates in
  if candidates <> [] then (
    Logger.debug "Eliminating %s >> %s" (SL.Term.show term) (SL.Term.show @@ List.hd candidates);
    Some (List.hd candidates))
  else None*)

let rec elim_heap_term phi g term : SL.Term.t = match SL.Term.view term with
  | HeapTerm (field, base) ->
    let base' = elim_heap_term phi g base in
    Logger.debug "Base for %a: %a" SL.Term.pp term SL.Term.pp base';
    SL.select_subformulae (SL.is_pointer) phi
    |> List.map SL.as_pointer
    |> List.find_map (fun (src, c, dsts) ->
          if SL_graph.must_eq g src base' then
            Option.some @@ StructDef.field_value c field dsts
          else None
        )
    |> BatOption.default_delayed (fun _ -> raise @@ NotSelfFramed term)
  | Var _ | SmtTerm _ -> term
  | BlockBegin t -> SL.Term.mk_block_begin @@ elim_heap_term phi g t
  | BlockEnd t -> SL.Term.mk_block_end @@ elim_heap_term phi g t

let apply_sh phi =
  let phi_aux = SL.map_view (function Exists (xs, phi) -> `Modify phi | _ -> `Skip) phi in
  let g = SL_graph.compute ~predicates:false phi_aux in
  SL.map_terms (elim_heap_term phi_aux g) phi

let apply_aux phi =
  match SL.view phi with
  | Or psis when List.for_all SL.is_symbolic_heap psis -> SL.mk_or @@ List.map apply_sh psis
  | _ when SL.is_symbolic_heap phi -> apply_sh phi
  | _ -> assert false

let rec apply phi =
  let phi' = apply_aux phi in
  if SL.equal phi phi' then phi
  else apply phi'
