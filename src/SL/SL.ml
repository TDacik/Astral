(* Implementation of separation logic over BaseLogic.
 *
 * TODO: do not continue under atoms in map/map_view.
 *         -> Using functor with input is_atom?
 *         -> Modify map to return an action?
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open MemoryModel

module A = BaseLogic.Application
module B = BaseLogic

include BaseLogic

let to_base_logic = Fun.id
let of_base_logic = Fun.id

module Variable = struct

  include Variable

  let is_pure var = not @@ Sort.is_loc @@ Variable.get_sort var

end

module Term = struct

  include BaseLogic
  include BaseLogic.SeparationLogic

  let mk_smt = SMT.to_base_logic

  type view =
    | Var of Variable.t
    | HeapTerm of Field.t * t
    | SmtTerm of SMT.t
    | IfEqual of t list * t * t (* Simplified version of IfThenElse*)
    | BlockBegin of t
    | BlockEnd of t

  let view = function
    | B.Variable var when Variable.is_loc var ->
      Var (Variable.mk (Variable.show var) (Variable.get_sort var))
    | B.Variable var -> SmtTerm (SMT.mk_var (Variable.show var) (Variable.get_sort var))
    | B.Application (IfThenElse, [c; t; e]) -> begin match c with
      | B.Application (Equal, xs) -> IfEqual (xs, t, e)
      | _ -> failwith "TODO: SL.Term.ite (general)"
      end
    | B.Application (BlockBegin, [x]) -> BlockBegin x
    | B.Application (BlockEnd, [x]) -> BlockEnd x
    | B.Application (HeapTerm field, [x]) -> HeapTerm (field, x)
    | x -> SmtTerm (SMT.of_base_logic x) (* TODO: checks *)

  let mk_if_equal xs t e = B.Boolean.mk_ite (B.Boolean.mk_eq xs) t e

  let is_nil term = match view term with
    | Var var -> Variable.is_nil var
    | _ -> false

  let is_heap_term t = match view t with HeapTerm _ -> true | _ -> false
  let is_smt_term t = match view t with SmtTerm _ -> true | _ -> false

  let as_var t = match view t with Var v -> v

  let rec get_vars t = match view t with
    | Var v -> [v]
    | SmtTerm t -> List.map (fun v -> Variable.of_description @@ SMT.Variable.describe v) (SMT.free_vars t)
    | HeapTerm (_, t) | BlockBegin t | BlockEnd t -> get_vars t
    | IfEqual (xs, t, e) -> List.concat_map get_vars (t :: e :: xs)

  let mem_var (v : Variable.t) (t : t) = BatList.mem_cmp Variable.compare v (get_vars t)

  let get_subterm t = match view t with
    | HeapTerm (_, t) | BlockBegin t | BlockEnd t -> t
    | _ -> raise @@ Invalid_argument ("SL.Term.get_subterm: " ^ show t)

end


(** Separation Logic *)

let to_smt : t -> SMT.t = SMT.of_base_logic
let of_smt : SMT.t -> t = SMT.to_base_logic

let of_term = Fun.id
let to_term = Fun.id

type view =
  (* Atoms *)
  | Emp
  | True
  | False
  | Pure of SMT.t
  | Eq of Term.t list
  | Distinct of Term.t list
  | PointsTo of Term.t * StructDef.t * Term.t list
  | Predicate of string * Term.t list * StructDef.t list

  (* Boolean connectives *)
  | And of t list
  | Or of t list
  | Not of t
  | GuardedNeg of t * t
  | Ite of t * t * t

  (* Quantifiers *)
  | Exists of Variable.t list * t
  | Forall of Variable.t list * t

  (* Spatial connectives *)
  | Star of t list
  | Septraction of t * t

let view phi =
  match phi with
  | B.Variable var ->
    (* Location variable always appear as SL.Term.Var
    assert (not @@ Variable.is_loc var); *)
    Pure (to_smt phi)
  | B.Application (app, xs) -> begin match app with
    | A.Emp -> Emp
    | A.Constant (Bool true) -> True
    | A.Constant (Bool false) -> False
    | A.Equal -> Eq xs
    | A.Distinct -> Distinct xs
    | A.PointsTo ->
      begin match xs with
        | [x; B.Application (A.Constructor s, ys)] -> PointsTo (x, s, ys)
        |  _ ->
          Exceptions.internal_error
            ~reason: "Unexpected pointer expression in SL formula"
            ~details: (show phi)
      end
    | A.Predicate (p, defs) -> Predicate (Identifier.show p, xs, defs)
    | A.And -> And xs
    | A.GuardedNot -> GuardedNeg (List.nth xs 0, List.nth xs 1)
    | A.Or -> Or xs
    | A.Implies -> Or [B.Boolean.mk_not @@ List.nth xs 0; List.nth xs 1]
    | A.Iff ->
      let lhs = List.nth xs 0 in
      let rhs = List.nth xs 1 in
      And [B.Boolean.mk_implies lhs rhs; B.Boolean.mk_implies rhs lhs]
    | A.IfThenElse -> Ite (List.nth xs 0, List.nth xs 1, List.nth xs 2)
    | A.Not -> Not (List.hd xs)
    | A.Star -> Star xs
    | A.Septraction -> Septraction (List.nth xs 0, List.nth xs 1)
    | A.Pure -> Pure (to_smt @@ List.hd xs)

    (** This case should happen only under Pure application *)
    | _ -> Pure (to_smt phi)
    end
  | B.Binder (Exists None, vs, x) -> Exists (vs, x)
  | B.Binder (Forall None, vs, x) -> Forall (vs, x)
  |  _ ->
    Exceptions.internal_error
      ~reason: "Unexpected construction in SL formula"
      ~details: (show phi)

let rec map_view fn phi =
  let apply phi = match fn @@ view phi with
    | `Modify res -> res
    | `Skip -> phi
  in
  match phi with
  | B.Variable _ -> apply phi
  | B.Application (app, psis) ->
    let args = List.map (map_view fn) psis in
    apply @@ Application (app, args)
  | B.Binder (binder, vs, psi) ->
    let body = map_view fn psi in
    apply @@ Binder (binder, vs, body)

include BaseLogic.Boolean
include BaseLogic.SeparationLogic

(** Hide range parameters *)
include BaseLogic.Quantifiers
let mk_forall vars phi = BaseLogic.Quantifiers.mk_forall vars phi
let mk_exists vars phi = BaseLogic.Quantifiers.mk_exists vars phi

(** Shadow Boolean.mk_var *)
let mk_var = BaseLogic.mk_var
let mk_fresh_var = BaseLogic.mk_fresh_var

(** Redefinition with compatible types *)
let mk_pure smt =
  let smt = SMT.of_base_logic @@ SMT.to_base_logic smt in
  SeparationLogic.mk_pure (of_smt smt)

let mk_not phi = BaseLogic.mk_app Not [phi]

let rec negate_pure phi = match view phi with
  | Distinct xs -> mk_eq xs
  | Eq xs -> mk_distinct xs
  | Star xs -> mk_or @@ List.map negate_pure xs (* Treat star as classical conjunction *)

(** Simplify to emp, instead of true *)
let mk_eq xs =
  if !BaseLogic.do_simplification then match mk_eq xs with
    | res when BaseLogic.equal res tt -> emp
    | res -> res
  else mk_eq xs

let mk_ite cond b_then b_else =
  if !BaseLogic.do_simplification then begin
    if equal cond tt || equal cond emp then b_then
    else if equal cond ff then b_else
    else if equal b_then ff then mk_star [negate_pure cond; b_else]
    else if equal b_else ff then mk_star [cond; b_then]
    else BaseLogic.Boolean.mk_ite cond b_then b_else
  end
  else BaseLogic.Boolean.mk_ite cond b_then b_else

(** Copied from BaseLogic *)
let mk_mupliple_ite branches t_else = match branches with
  | (c, t) :: rest -> mk_ite c t (mk_multiple_ite rest t_else)
  | [] -> t_else


(** Redefine to do not continue under atoms *)
let rec select_subformulae pred phi =
  let acc = match phi with
    | Variable _ -> []
    | Application ((PointsTo | Predicate _ | Equal | Distinct | Pure), _) -> [] (* Stop *)
    | Application (_, xs) -> BatList.concat_map (select_subformulae pred) xs
    | Binder (_, _, x) -> select_subformulae pred x
  in
  if pred phi then phi :: acc else acc


let free_vars ?(with_nil=true) ?(with_pure=false) phi =
  let vars = free_vars phi in
  let vars =
    if with_nil then vars
    else BatList.remove_if Variable.is_nil vars
  in
  if not @@ with_pure then List.filter Variable.is_loc vars
  else vars


let is_atom phi = match view phi with
  | Eq _ | Distinct _ | PointsTo _ | Predicate _ | Emp -> true
  | _ -> false

let is_pure_atom phi = match view phi with
  | Eq _ | Distinct _ -> true
  | _ -> false

let is_emp phi = match view phi with
  | Emp -> true
  | _ -> false

let is_pointer phi = match view phi with
  | PointsTo _ -> true
  | _ -> false

let is_predicate phi = match view phi with
  | Predicate _ -> true
  | _ -> false

let is_spatial_atom phi = match view phi with
  | Predicate _ | PointsTo _ | Emp -> true
  | _ -> false

let is_atomic =
  for_all_apps (function
    | Predicate _ -> false
    | _ -> true
  )

(** Note: emp is not pure as it restricts the heap *)
let is_pure psi = match psi with
  | Variable v -> Variable.is_pure v
  | _ ->
    for_all_apps (function
      | Predicate _ | PointsTo | Star | Septraction | Emp -> false
      | _ -> true
    ) psi

let is_pure_smt phi =
  is_pure phi && List.for_all (Variable.is_pure) (free_vars phi)

let is_low_level = exists_app (function BlockBegin | BlockEnd -> true | _ -> false)

(* TODO: will require normalisation for user-defined *)
let get_root phi = match view phi with
  | Predicate (_, x :: _, _) -> x

let get_struct_def phi = match view phi with
  | PointsTo (_, def, _) -> def

let get_fields phi =
  select_subformulae is_pointer phi
  |> List.map get_struct_def
  |> List.concat_map StructDef.get_fields

let get_terms phi =
  let subformulae = select_subformulae is_atom phi in
  let get_terms_aux psi = match view psi with
    | Emp -> []
    | Distinct xs | Eq xs | Predicate (_, xs, _) -> xs
    | PointsTo (x, _, ys) -> x :: ys
  in
  let terms = List.concat_map get_terms_aux subformulae in
  BatList.unique ~eq:equal terms

let get_loc_terms ?(with_free_vars=true) phi heap_sort =
  let loc_sorts = HeapSort.get_loc_sorts heap_sort in
  let res =
    get_terms phi
    |> List.filter (fun term -> BatList.mem_cmp Sort.compare (Term.get_sort term) loc_sorts)
  in
  if with_free_vars then res
  else
    let bound_vars = bound_vars phi in
    List.filter (fun term -> List.for_all (fun v -> not @@ Term.mem_var v term) bound_vars) res

let get_terms_of_sort sort phi =
  get_terms phi
  |> List.filter (has_sort sort)

let rec is_symbolic_heap phi = match phi with
  | Variable _ -> true
  | Application _ when is_atom phi -> true
  | Application (And, psis) -> List.for_all is_pure_atom psis
  | Application (Star, psis) -> List.for_all is_symbolic_heap psis
  | Binder (Exists _, _, psi) -> is_symbolic_heap psi
  | _ -> false

let is_symbolic_heap_entl phi = match view phi with
  | GuardedNeg (lhs, rhs) -> is_symbolic_heap lhs && is_symbolic_heap rhs
  | _ -> false

let is_positive =
  for_all (function
    | Application (Not, _) -> false
    | _ -> true
  )

let is_negation_free =
  for_all (function
    | Application (Not, _) | Application (GuardedNot, _) -> false
    | _ -> true
  )

let as_equality phi = match view phi with
  | Eq xs -> Some xs
  | _ -> None

let as_pointer phi = match view phi with
  | PointsTo (x, def, ys) -> (x, def, ys)
  | _ -> raise @@ Invalid_argument "Not a pointer"

let as_predicate phi = match view phi with
  | Predicate (name, ys, _) -> (name, ys)
  | _ -> raise @@ Invalid_argument "Not a predicate"

let as_symbolic_heap phi = match view phi with
  | Star psis -> List.partition is_pure psis
  | And psis -> psis, []
  | _ when is_pure phi -> [phi], []
  | _ -> [], [phi]

let as_symbolic_heap' phi =
  let pure, spatial = as_symbolic_heap phi in
  pure @ spatial

let rec as_quantified_symbolic_heap phi = match view phi with
  | _ when is_atom phi -> [], [phi]
  | And atoms when List.for_all is_pure_atom atoms -> [], atoms
  | Star atoms ->
    List.fold_left (fun acc atom ->
      let qs, atoms = as_quantified_symbolic_heap atom in
      qs @ fst acc, atoms @ snd acc
    ) ([], []) atoms
  | Exists (xs, body) ->
    let qs, atoms = as_quantified_symbolic_heap body in
    xs @ qs, atoms
  | _ -> raise @@ Invalid_argument ("Not a symbolic heap: " ^ (show phi))

let as_entailment phi = match view phi with
  | GuardedNeg (lhs, rhs) -> (lhs, rhs)
  | _ -> raise @@ Invalid_argument ("Not an entailment " ^ show phi)

type fragment =
  | SymbolicHeap_SAT
  | SymbolicHeap_ENTL
  | Positive
  | Arbitrary
  | Atomic

let show_fragment = function
  | SymbolicHeap_SAT -> "SH_SAT"
  | SymbolicHeap_ENTL -> "SH_ENTL"
  | Positive -> "positive"
  | Atomic -> "atomic"
  | Arbitrary -> "arbitrary"

let classify_fragment phi =
  if is_atomic phi then Atomic
  else if is_symbolic_heap phi then SymbolicHeap_SAT
  else if is_symbolic_heap_entl phi then SymbolicHeap_ENTL
  else if is_positive phi then Positive
  else Arbitrary

type query =
  | SymbolicHeap_SAT of t
  | SymbolicHeap_ENTL of t * t
  | Arbitrary of t

let as_query phi =
  if is_symbolic_heap phi then
    SymbolicHeap_SAT phi
  else if is_symbolic_heap_entl phi then
    let lhs, rhs = as_entailment phi in
    if is_symbolic_heap lhs && is_symbolic_heap rhs then
      SymbolicHeap_ENTL (lhs, rhs)
    else
      Arbitrary phi
  else
    Arbitrary phi

let translate_pure_with_heap_term (fn : Term.t -> SMT.t) phi =
  to_base_logic phi
  |> BaseLogic.map
      (fun t -> match t with
        | Variable _ -> of_smt @@ fn t
        | Application (A.HeapTerm _, _) -> of_smt (fn t)
        | x -> x
      )
  |> SMT.of_base_logic

module Infix = struct

  let (==) x y = mk_eq [x; y]
  let (!=) x y = mk_distinct [x; y]
  let (|->) = mk_pto
  let (|=>) = mk_pto_tuple

  let (=>)  = mk_implies
  let (<=>) x y = mk_iff [x; y]
  let (&!)  = mk_gneg
  let (&&) x y  = mk_and [x; y]
  let (||) x y = mk_or [x; y]

  let ( * ) x y = mk_star [x; y]

end
