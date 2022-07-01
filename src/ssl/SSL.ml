(* Syntax of strong-separation logic
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Sort = struct

  type t =
    | Loc
    | Int
    | Bool
  [@@deriving compare, equal]

  let show = function
    | Loc -> "Loc"
    | Int -> "Int"

  module Self = struct
    type nonrec t = t
    let show = show
  end

  include Datatype.Printable(Self)

end

module Variable = struct

  let compare_int = Int.compare
  let equal_int   = Int.equal

  (*TODO: move to context *)
  module IDS = Hashtbl.Make
    (struct
      include String
      let hash = Hashtbl.hash
    end)

  let id = ref 1

  let ids = ref (IDS.create 97)

  type var = {
    id : int;
    sort : Sort.t;
    name : string [@compare.ignore] [@equal.ignore];
  } [@@deriving compare, equal]

  type t =
    | Var of var
    | Term of SMT.Term.t
    | Nil
  [@@deriving compare, equal]

  let is_var = function
    | Var _ | Nil -> true
    | Term _ -> false

  let mk_fresh name =
    let fresh_id = !id in
    id := !id + 1;
    let var = Var {id = fresh_id; sort = Sort.Loc; name = name} in
    IDS.add !ids name fresh_id;
    var

  let mk name =
    try
      let id = IDS.find !ids name in
      Var {id = id; sort = Sort.Loc; name = name}
    with Not_found -> mk_fresh name

  let mk_int name =
    try
      let id = IDS.find !ids name in
      Var {id = id; sort = Sort.Int; name = name}
    with Not_found -> mk_fresh name

  let show = function
    | Var var -> var.name
    | Term t -> SMT.Term.show t
    | Nil -> "nil"

  let hash v = match v with
    | Var var -> Hashtbl.hash var.id
    | Term t -> Hashtbl.hash (show v)
    | Nil -> Hashtbl.hash 0

  let is_nil = function
    | Var _ | Term _ -> false
    | Nil -> true

  (* ==== Datatype ==== *)

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end

type formula =
  | And of formula * formula
  | Or of formula * formula
  | Not of formula
  | GuardedNeg of formula * formula
  | Star of formula * formula
  | Septraction of formula * formula
  | LS of Variable.t * Variable.t
  | PointsTo of Variable.t * Variable.t
  | Eq of Variable.t * Variable.t
  | Neq of Variable.t * Variable.t

type t = formula

(* Intentional use of physical equality *)
let compare = Stdlib.compare
let equal x y = (Stdlib.compare x y) == 0

type arity =
  | Atom of Variable.t * Variable.t
  | Unary of formula
  | Binary of formula * formula

let get_arity = function
  | And (f1, f2) -> Binary (f1, f2)
  | Or (f1, f2) -> Binary (f1, f2)
  | Not f -> Unary f
  | GuardedNeg (f1, f2) -> Binary (f1, f2)
  | Star (f1, f2) -> Binary (f1, f2)
  | Septraction (f1, f2) -> Binary (f1, f2)
  | LS (v1, v2) -> Atom (v1, v2)
  | PointsTo (v1, v2) -> Atom (v1, v2)
  | Eq (v1, v2) -> Atom (v1, v2)
  | Neq (v1, v2) -> Atom (v1, v2)

let is_atom phi = match get_arity phi with
  | Atom _ -> true
  | _ -> false

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

(** Size is number of nodes in AST *)
let rec size phi = match get_arity phi with
  | Atom _ -> 1
  | Unary phi -> 1 + size phi
  | Binary (phi1, phi2) -> 1 + size phi1 + size phi2

(** Fold using preorder traversal of AST *)
let rec fold (fn : t -> 'a -> 'a) phi (acc : 'a) = match get_arity phi with
  | Atom _ -> fn phi acc
  | Unary psi1 -> fn phi (fold fn psi1 acc)
  | Binary (psi1, psi2) -> fn phi (fold fn psi1 (fold fn psi2 acc))

exception NotSubformula

(* TODO: more effectively *)
let subformula_id ?(physically=true) phi psi =
  let rec subformula_id phi psi id =
    let eq = if physically then (==) else (=) in
    if eq phi psi then (Some id, id)
    else match get_arity phi with
    | Atom _ -> (None, id)
    | Unary phi' -> subformula_id phi' psi (id+1)
    | Binary (phi1, phi2) -> match subformula_id phi1 psi (id+1) with
      | (None, id) -> subformula_id phi2 psi (id+1)
      | res -> res
  in
  match subformula_id phi psi 0 with
  | (None, _) -> raise NotSubformula
  | (Some id, _) -> id

(** ==== Fragment classification ==== *)

let rec is_negation_free phi = match get_arity phi with
  | Atom _ -> true
  | Unary psi -> false
  | Binary (psi1, psi2) -> begin match phi with
    | GuardedNeg _ -> false
    | _ -> is_negation_free psi1 && is_negation_free psi2
  end

let rec is_symbolic_heap phi = match get_arity phi with
  | Atom _ -> true
  | Unary psi -> false
  | Binary _ -> begin match phi with
    | Star (psi1, psi2) -> is_symbolic_heap psi1 && is_symbolic_heap psi2
    | _ -> false
  end

let is_symbolic_heap_entl phi = match phi with
  | GuardedNeg (psi1, psi2) -> is_symbolic_heap psi1 && is_symbolic_heap psi2
  | _ -> false

let rec is_positive phi = match get_arity phi with
  | Atom _ -> true
  | Unary _ -> false
  | Binary (psi1, psi2) -> is_positive psi1 && is_positive psi2

let rec has_unique_shape phi = match phi with
  | Eq _ | Neq _ | PointsTo _ -> true
  | And (psi1, psi2) | Star (psi1, psi2) -> has_unique_shape psi1 && has_unique_shape psi2

type fragment =
  | SymbolicHeap_SAT
  | SymbolicHeap_ENTL
  | Positive
  | Arbitrary

let classify_fragment phi =
  if is_symbolic_heap phi then SymbolicHeap_SAT
  else if is_symbolic_heap_entl phi then SymbolicHeap_ENTL
  else if is_positive phi then Positive
  else Arbitrary

let rec has_unique_footprint = function
  | Eq _ | Neq _ | PointsTo _ | LS _ -> true
  | And (f1, f2) -> has_unique_footprint f1 || has_unique_footprint f2
  | Or (f1, f2) -> has_unique_footprint f1 && has_unique_footprint f2
  | GuardedNeg (f1, f2) -> has_unique_footprint f1
  | Star (f1, f2) -> has_unique_footprint f1 && has_unique_footprint f2
  | Septraction (f1, f2) -> has_unique_footprint f1 && has_unique_footprint f2
  | Not f -> false

let rec _get_vars f = match f with
  | And (f1, f2) -> (_get_vars f1) @ (_get_vars f2)
  | Or (f1, f2) -> (_get_vars f1) @ (_get_vars f2)
  | Not f -> _get_vars f
  | GuardedNeg (f1, f2) -> (_get_vars f1) @ (_get_vars f2)
  | Star (f1, f2) -> (_get_vars f1) @ (_get_vars f2)
  | Septraction (f1, f2) -> (_get_vars f1) @ (_get_vars f2)
  | LS (v1, v2) -> List.filter Variable.is_var [v1; v2]
  | PointsTo (v1, v2) -> List.filter Variable.is_var [v1; v2]
  | Eq (v1, v2) -> List.filter Variable.is_var [v1; v2]
  | Neq (v1, v2) -> List.filter Variable.is_var [v1; v2]

let get_vars ?(with_nil=true) f =
  let vars = if with_nil then Variable.Nil :: _get_vars f else _get_vars f in
  List.sort_uniq Variable.compare vars

(* ==== Constructors ==== *)

let mk_eq x y = Eq (x, y)
let mk_neq x y = Neq (x, y)
let mk_pto x y = PointsTo (x, y)
let mk_ls x y = LS (x, y)

let mk_not phi = Not phi
let mk_emp () = Eq (Variable.Nil, Variable.Nil)
let mk_not_emp () = Not (Eq (Variable.Nil, Variable.Nil))
let mk_false () = GuardedNeg (mk_emp (), mk_emp ())
let mk_true () = Not (mk_false ())

let mk_distinct operands = match operands with
  | [] -> mk_true ()
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

let rec show = function
  | And (f1, f2) -> Format.asprintf "(%s) ∧ (%s)" (show f1) (show f2)
  | Or (f1, f2) -> Format.asprintf "(%s) ∨ (%s)" (show f1) (show f2)
  | Not f -> Format.asprintf "¬ (%s)" (show f)
  | GuardedNeg (f1, f2) -> Format.asprintf "(%s) ∧¬ (%s)" (show f1) (show f2)
  | Star (f1, f2) -> Format.asprintf "(%s) ★ (%s)" (show f1) (show f2)
  | Septraction (f1, f2) -> Format.asprintf "(%s) --(★) (%s)" (show f1) (show f2)
  | LS (v1, v2) -> Format.asprintf "ls(%a, %a)" Variable.pp v1 Variable.pp v2
  | PointsTo (v1, v2) -> Format.asprintf "%a ↦ %a" Variable.pp v1 Variable.pp v2
  | Eq (v1, v2) -> Format.asprintf "%a = %a" Variable.pp v1 Variable.pp v2
  | Neq (v1, v2) -> Format.asprintf "%a ≠ %a" Variable.pp v1 Variable.pp v2

(* ==== Datatype ==== *)

module Self = struct
  type nonrec t = t
  let show = show
  let compare = compare
end

include Datatype.Printable(Self)
include Datatype.Collections(Self)
