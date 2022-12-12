(* Syntax of strong-separation logic
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Logic_sig

module Variable = struct
  include Variable.Make( )

  let nil = mk "nil" Sort.Loc

  let is_nil var = equal var nil

  let mk name = mk name Sort.Loc

  let mk_fresh name = mk_fresh name Sort.Loc

  let hash = Hashtbl.hash

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end

(** TODO: consider variadic and, or and star + eq/neq *)
type t =
  | Var of Variable.t     (* Location variable *)
  | Pure of SMT.Term.t    (* Pure boolean term which does not contain location variables *)

  (* Atoms *)
  | Eq of t * t
  | Neq of t * t
  | PointsTo of t * t
  | LS of t * t
  | DLS of t * t

  (* Boolean connectives *)
  | And of t * t
  | Or of t * t
  | Not of t
  | GuardedNeg of t * t

  (* Spatial connectives *)
  | Star of t * t
  | Septraction of t * t

let compare = Stdlib.compare
let equal x y = (Stdlib.compare x y) == 0
let hash = Hashtbl.hash

let describe_node : t -> t node_info = function
  | Var x -> (Variable.show_with_sort x, Var (Variable.show x, Sort.Loc))
  | Pure t -> ("pure " ^ SMT.show t, Operator ([], (SMT.get_sort t)))
  | Eq (x, y) -> ("=", Operator ([x; y], Sort.Bool))
  | Neq (x, y) -> ("neq", Operator ([x; y], Sort.Bool))
  | PointsTo (x, y) -> ("pto", Operator ([x; y], Sort.Bool))
  | LS (x, y) -> ("ls", Operator ([x; y], Sort.Bool))
  | DLS (x, y) -> ("dls", Operator ([x; y], Sort.Bool))
  | And (psi1, psi2) -> ("and", Connective [psi1; psi2])
  | Or (psi1, psi2) -> ("or", Connective [psi1; psi2])
  | Not psi -> ("not", Connective [psi])
  | GuardedNeg (psi1, psi2) -> ("gneg", Connective [psi1; psi2])
  | Star (psi1, psi2) -> ("star", Connective [psi1; psi2])
  | Septraction (psi1, psi2) -> ("septraction", Connective [psi1; psi2])

(* First, build implementation of logical functions (including show). *)

module Self = struct
  type nonrec t = t
  let describe_node = describe_node
end

include Logic.Make(Self)

(* Second, build implementation of common operations. *)
module Self2 = struct
  include Self
  let show = show
  let compare = compare
end

include Datatype.Printable(Self2)
include Datatype.Collections(Self2)

let is_var = function
  | Var _ -> true
  | _ -> false

let is_atom phi = match node_type phi with
  | Operator _ -> true
  | _ -> false

let rec is_pure phi = match node_type phi with
  | Var _ -> false
  | Operator ([], _) -> true
  | Operator (terms, _) | Connective terms -> List.for_all is_pure terms

(** Transform suitable negations to guarded form *)
let rec _normalise = function
  | And (f1, f2) -> begin match f1, f2 with
    | Not g1, Not g2 -> Not (Or (_normalise g1, _normalise g2))
    | Not g1, g2     -> _normalise (GuardedNeg (g2, g1))
    | g1, Not g2     -> _normalise (GuardedNeg (g1, g2))
    | _, _           -> And (_normalise f1, _normalise f2)
  end
  | Or (f1, f2) -> begin match f1, f2 with
    | Not g1, Not g2 -> Not (And (_normalise g1, _normalise g2))
    | Not g1, g2     -> Not (GuardedNeg (_normalise g1, _normalise g2)) (*~g1 \/ g2 --> ~ (g1 /\ ~g2 *)
    | g1, Not g2     -> Not (GuardedNeg (_normalise g2, _normalise g1)) (*~g1 \/ g2 --> ~ (g1 /\ ~g2 *)
    | _, _           -> Or (_normalise f1, _normalise f2)
  end
  | GuardedNeg (f1, f2) -> begin match f1, f2 with
    | Not g1, Not g2 -> GuardedNeg (_normalise g2, _normalise g1)       (*~g1 /\_ ~g2 --> g2 /\_ g1 *)
    | Not g1, g2     -> Not (Or (_normalise g1, _normalise g2))
    | g1, Not g2     -> And (_normalise g1, _normalise g2)
    | _, _           -> GuardedNeg (_normalise f1, _normalise f2)
  end
  | Not phi -> begin match phi with
    | Not psi -> _normalise psi        (* Double negation elimination *)
    | _ -> Not (_normalise phi)
  end
  | Star (f1, f2) -> Star (_normalise f1, _normalise f2)
  | Septraction (f1, f2) -> Septraction (_normalise f1, _normalise f2)
  | atom -> atom

(** Perform normalisation until fixpoint is reached *)
let rec normalise phi =
  let phi' = _normalise phi in
  if equal phi' phi then phi'
  else normalise phi'

(** What exactly is the chunk size of single variable? *)
let rec chunk_size = function
  | Eq _ | Neq _ | PointsTo _ | LS _ | DLS _ | Var _ -> 1
  | Star (psi1, psi2) -> chunk_size psi1 + chunk_size psi2
  | Septraction (_, psi2) -> chunk_size psi2
  | And (psi1, psi2) | Or (psi1, psi2) | GuardedNeg (psi1, psi2) ->
      max (chunk_size psi1) (chunk_size psi2)
  | Not psi -> chunk_size psi

(** Fold using preorder traversal of AST *)
let rec fold (fn : t -> 'a -> 'a) phi (acc : 'a) = match node_type phi with
  | Var _ -> fn phi acc
  | Operator (terms, _) | Connective terms ->
      List.fold_left (fun acc term -> fold fn phi acc) acc terms

exception NotSubformula

(* TODO: more effectively *)
let subformula_id ?(physically=true) phi psi =
  let rec subformula_id phi psi id =
    let eq = if physically then (==) else (=) in
    if eq phi psi then (Some id, id)
    else match node_type phi with
    | Var _ | Operator _ -> (None, id)
    | Connective [t1] -> subformula_id t1 psi (id+1)
    | Connective [t1; t2] -> match subformula_id t1 psi (id+1) with
      | (None, id) -> subformula_id t2 psi (id+1)
      | res -> res
  in
  match subformula_id phi psi 0 with
  | (None, _) -> raise NotSubformula
  | (Some id, _) -> id

(** Find subformula with given ID. *)
let rec find_by_id phi psi id =
  let phi_id = subformula_id phi psi in
  if Int.equal phi_id id then psi
  else match node_type phi with
  | Var _ | Operator _ -> raise Not_found
  | Connective [t1] -> find_by_id phi t1 id
  | Connective [t1; t2] ->
      try find_by_id phi t1 id
      with Not_found -> find_by_id phi t2 id

let find_by_id phi id = find_by_id phi phi id

(** ==== Fragment classification ==== *)

let rec is_negation_free phi = match node_type phi with
  | Var _ | Operator _ -> true
  | Connective terms ->
    begin match phi with
      | GuardedNeg _ | Not _ -> false
      | _ -> List.for_all is_negation_free terms
    end

let rec is_symbolic_heap phi = match node_type phi with
  | Var _ | Operator _ -> true
  | _ ->
    begin match phi with
      | Star (psi1, psi2) -> is_symbolic_heap psi1 && is_symbolic_heap psi2
      | _ -> false
    end

let is_symbolic_heap_entl phi = match phi with
  | GuardedNeg (psi1, psi2) -> is_symbolic_heap psi1 && is_symbolic_heap psi2
  | _ -> false

let rec is_positive phi = match node_type phi with
  | Var _ | Operator _ -> true
  | Connective terms ->
      begin match phi with
        | Not _ -> false
        | _ -> List.for_all is_positive terms
      end

let rec has_unique_shape phi = match phi with
  | Eq _ | Neq _ | PointsTo _ -> true
  | And (psi1, psi2) | Star (psi1, psi2) -> has_unique_shape psi1 && has_unique_shape psi2
  | _ -> false

let rec is_atomic phi = match node_type phi with
  | Var _ -> true
  | Operator _ ->
    begin match phi with
      | LS _ | DLS _ -> false
      | _ -> true
    end
  | Connective terms -> List.for_all is_atomic terms

type fragment =
  | SymbolicHeap_SAT
  | SymbolicHeap_ENTL
  | Atomic
  | Positive
  | Arbitrary

let classify_fragment phi =
  if is_symbolic_heap phi then SymbolicHeap_SAT
  else if is_symbolic_heap_entl phi then SymbolicHeap_ENTL
  else if is_atomic phi then Atomic
  else if is_positive phi then Positive
  else Arbitrary

let rec has_unique_footprint = function
  | Eq _ | Neq _ | PointsTo _ | LS _ | DLS _ -> true
  | And (f1, f2) -> has_unique_footprint f1 || has_unique_footprint f2
  | Or (f1, f2) -> has_unique_footprint f1 && has_unique_footprint f2
  | GuardedNeg (f1, f2) -> has_unique_footprint f1
  | Star (f1, f2) -> has_unique_footprint f1 && has_unique_footprint f2
  | Septraction (f1, f2) -> has_unique_footprint f1 && has_unique_footprint f2
  | Not f -> false

(** TODO: rename *)
let get_vars ?(with_nil=true) phi =
  let vars =
    free_vars phi
    |> List.filter is_var
    |> List.map (fun v -> match v with Var v -> v)
  in
  if with_nil
  then vars
  else BatList.remove vars Variable.nil

(* ==== Constructors ==== *)

let mk_eq x y = Eq (x, y)
let mk_neq x y = Neq (x, y)
let mk_pto x y = PointsTo (x, y)
let mk_ls x y = LS (x, y)
let mk_dls x y = DLS (x, y)

let mk_not phi = Not phi

let mk_implies lhs rhs = Or (Not lhs, rhs)
let mk_iff lhs rhs = And (mk_implies lhs rhs, mk_implies rhs lhs)
let mk_gneg lhs rhs = GuardedNeg (lhs, rhs)
let mk_emp () = Eq (Var Variable.nil, Var Variable.nil)
let mk_not_emp () = Not (Eq (Var Variable.nil, Var Variable.nil))
let mk_false () = GuardedNeg (mk_emp (), mk_emp ())
let mk_true () = Not (mk_false ())

let mk_distinct operands = match operands with
  | [] -> mk_true ()
  | [x; y] -> mk_neq x y
  | fst :: tail -> failwith "not implemented"

let mk_and operands = match operands with
  | [] -> mk_true ()
  | fst :: tail -> List.fold_left (fun phi op -> And (phi, op)) fst tail

let mk_or operands = match operands with
  | [] -> mk_false ()
  | fst :: tail -> List.fold_left (fun phi op -> Or (phi, op)) fst tail

let mk_star operands = match operands with
  | [] -> mk_emp ()
  | fst :: tail -> List.fold_left (fun phi op -> Star (phi, op)) fst tail

let mk_septraction lhs rhs = Septraction (lhs, rhs)

let mk_wand lhs rhs = Not (Septraction (lhs, Not rhs))

let fold_on_vars fn acc phi =
  let vars = get_vars phi in
  List.fold_right fn vars acc

let rec iter_on_subformulas fn phi =
  let iter = iter_on_subformulas fn in
  match phi with
  | And (f1, f2) -> iter f1; iter f2; fn phi
  | Or (f1, f2) -> iter f1; iter f2; fn phi
  | Not f -> iter f; fn phi
  | GuardedNeg (f1, f2) -> iter f1; iter f2; fn phi
  | Star (f1, f2) -> iter f1; iter f2; fn phi
  | Septraction (f1, f2) -> iter f1; iter f2; fn phi
  (* Atomic formulas *)
  | _ -> fn phi

module Var = struct
  module V = Variable

  let show (Var v) = V.show v
  let compare (Var v1) (Var v2) = V.compare v1 v2
  let equal (Var v1) (Var v2) = V.equal v1 v2

  let nil = Var V.nil
  let is_nil (Var (name, _)) = String.equal "nil" name


  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end

let mk_pure term = Pure term
let mk_pure_var name sort = Pure (SMT.Variable.mk name sort)
