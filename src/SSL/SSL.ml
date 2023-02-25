(* Syntax of strong-separation logic
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Logic_sig

module Variable = struct
  include Variable.Make( )

  let nil = mk "nil" Sort.Loc

  let is_nil var = equal var nil

  let mk_sort name sort = mk name sort

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

(** TODO: consider variadic and & or *)
type t =
  | Var of Variable.t     (* Location variable *)
  | Pure of SMT.Term.t    (* Pure boolean term which does not contain location variables *)

  (* Atoms *)
  | Eq of t list
  | Distinct of t list
  | PointsTo of t * t list
  | LS of t * t
  | DLS of t * t * t * t
  | SkipList of int * t * t

  (* Boolean connectives *)
  | And of t * t
  | Or of t * t
  | Not of t
  | GuardedNeg of t * t

  (* Quantifiers *)
  | Exists of t list * t
  | Forall of t list * t

  (* Spatial connectives *)
  | Star of t list
  | Septraction of t * t

let compare = Stdlib.compare
let hash = Hashtbl.hash

let describe_node : t -> t node_info = function
  | Var (name, Sort.Loc) -> (name, Var (name, Sort.Loc))
  | Pure t -> ("pure " ^ SMT.Term.show t, Operator ([], (SMT.Term.get_sort t)))
  | Eq xs -> ("=", Operator (xs, Sort.Bool))
  | Distinct xs -> ("distinct", Operator (xs, Sort.Bool))
  | PointsTo (x, ys) -> ("pto", Operator (x :: ys, Sort.Bool))
  | LS (x, y) -> ("ls", Operator ([x; y], Sort.Bool))
  | DLS (x, y, f, l) -> ("dls", Operator ([x; y; f; l], Sort.Bool))
  | SkipList (depth, x, y) -> (Format.asprintf "skl%d" depth, Operator ([x; y], Sort.Bool))
  | And (psi1, psi2) -> ("and", Connective [psi1; psi2])
  | Or (psi1, psi2) -> ("or", Connective [psi1; psi2])
  | Not psi -> ("not", Connective [psi])
  | GuardedNeg (psi1, psi2) -> ("gneg", Connective [psi1; psi2])
  | Exists (xs, psi) -> ("exists", Quantifier (xs, psi))
  | Forall (xs, psi) -> ("forall", Quantifier (xs, psi))
  | Star psis -> ("star", Connective psis)
  | Septraction (psi1, psi2) -> ("septraction", Connective [psi1; psi2])

(** First, build implementation of logical functions (including show). *)

module Self = struct
  type nonrec t = t
  let describe_node = describe_node
end

include Logic.Make(Self)

(** Second, build implementation of common operations. *)

module Self2 = struct
  include Self
  let show = show
  let compare = compare
end

include Datatype.Printable(Self2)
include Datatype.Comparable(Self2)
include Datatype.Collections(Self2)

let is_var = function
  | Var _ -> true
  | _ -> false

let is_atom phi = match node_type phi with
  | Operator _ -> true
  | _ -> false

(* ==== Basic constructors ==== *)

let mk_emp () = Eq [Var Variable.nil; Var Variable.nil]
let mk_not_emp () = Not (mk_emp ())
let mk_false () = GuardedNeg (mk_emp (), mk_emp ())
let mk_true () = Not (mk_false ())
let mk_not phi = Not phi

(* ==== Checkers ==== *)

let is_false phi = equal phi (mk_false ()) || equal phi (mk_not @@ mk_true ())
let is_true phi = equal phi (mk_true ()) || equal phi (mk_not @@ mk_false ())
let is_emp phi = equal phi (mk_emp ())

let get_implies_operands = function
  | Or (Not lhs, rhs) -> (lhs, rhs)
  | Or (rhs, Not lhs) -> (lhs, rhs)
  | _ -> raise (Invalid_argument "Not an implication")

let is_implies phi =
  try
    let _ = get_implies_operands phi in
    true
  with _ -> false

let get_iff_operands = function
  | And (lhs, rhs) when is_implies lhs && is_implies rhs ->
    let lhs1, lhs2 = get_implies_operands lhs in
    let rhs1, rhs2 = get_implies_operands rhs in
    if equal lhs1 rhs2 && equal rhs1 lhs2 then (lhs1, rhs1)
    else raise (Invalid_argument "Not an iff")
  | _ -> raise (Invalid_argument "Not an iff")

let is_iff phi =
  try
    let _ = get_iff_operands phi in
    true
  with _ -> false

(* ==== Constructors ==== *)

let mk_nil () = Var Variable.nil
let mk_var name = Var (Variable.mk name)
let mk_fresh_var name = Var (Variable.mk_fresh name)

let mk_eq x y = Eq [x; y]
let mk_eq_list xs = Eq xs

let mk_distinct x y = Distinct [x; y]
let mk_distinct_list xs = Distinct xs

let mk_pto x y = PointsTo (x, [y])
let mk_pto_seq x ys = PointsTo (x, ys)
let mk_ls x y = LS (x, y)
let mk_dls x y f l = DLS (x, y, f, l)
let mk_skl depth x y = SkipList (depth, x, y)

let mk_implies lhs rhs = Or (Not lhs, rhs)
let mk_gneg lhs rhs = GuardedNeg (lhs, rhs)

let mk_and operands = match operands with
  | [] -> mk_true ()
  | fst :: tail -> List.fold_left (fun phi op -> And (phi, op)) fst tail

let mk_iff operands = match operands with
  | [] -> mk_true ()
  | [psi] -> psi
  | [psi1; psi2] -> mk_and [mk_implies psi1 psi2; mk_implies psi2 psi1]
  | _ -> failwith "TODO"

let mk_bin_iff psi1 psi2 = mk_iff [psi1; psi2]

let mk_bin_and phi1 phi2 = mk_and [phi1; phi2]

let mk_or operands = match operands with
  | [] -> mk_false ()
  | fst :: tail -> List.fold_left (fun phi op -> Or (phi, op)) fst tail

let mk_bin_or phi1 phi2 = mk_or [phi1; phi2]

let mk_star operands = match operands with
  | [] -> mk_emp ()
  | [psi] -> psi
  | operands -> Star operands

let mk_bin_star phi1 phi2 = mk_star [phi1; phi2]

let mk_septraction lhs rhs = Septraction (lhs, rhs)

let mk_wand lhs rhs = Not (Septraction (lhs, Not rhs))

let mk_exists xs phi = match xs with
  | [] -> phi
  | xs -> Exists (xs, phi)
let mk_forall xs phi = match xs with
  | [] -> phi
  | xs -> Forall (xs, phi)

let rec is_pure phi = match phi with
  | Var _ | Distinct _ | Pure _ -> true
  | PointsTo _ | LS _ | DLS _ | SkipList _ | Star _ | Septraction _ | Not _ -> false
  | _ ->
    begin match node_type phi with
    | Operator (terms, _) | Connective terms -> List.for_all is_pure terms
    | Quantifier (_, psi) -> is_pure psi
    end

(** Formula is pure_smt if it does not contain location variables. *)
let rec is_pure_smt phi = match node_type phi with
  | Var (_, _) -> false
  | Operator (terms, _) | Connective terms -> List.for_all is_pure_smt terms
  | Quantifier (_, psi) -> is_pure_smt psi

let rec substitute_pure phi x term = match phi with
  | Var _ -> phi
  | Eq xs -> Eq xs
  | Distinct xs -> Distinct xs
  | PointsTo (x, ys) -> PointsTo (x, ys)
  | LS (x, y) -> LS (x, y)
  | DLS (x, y, f, l) -> DLS (x, y, f, l)
  | SkipList (depth, x, y) -> SkipList (depth, x, y)
  | And (psi1, psi2) -> And (substitute_pure psi1 x term, substitute_pure psi2 x term)
  | Or (psi1, psi2) -> Or (substitute_pure psi1 x term, substitute_pure psi2 x term)
  | Not psi -> Not (substitute_pure psi x term)
  | GuardedNeg (psi1, psi2) ->
      GuardedNeg (substitute_pure psi1 x term, substitute_pure psi2 x term)
  | Exists (xs, psi) -> Exists (xs, substitute_pure psi x term)
  | Forall (xs, psi) -> Forall (xs, substitute_pure psi x term)
  | Star psis -> Star (List.map (fun psi -> substitute_pure psi x term) psis)
  | Septraction (psi1, psi2) ->
      Septraction (substitute_pure psi1 x term, substitute_pure psi2 x term)
  (* Delegate to SMT substitution *)
  | Pure pure -> Pure (SMT.substitute pure x term)


(** Transform suitable negations to guarded form
 *  TODO: quantifiers, move to preprocessing *)
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
  | Star fs -> Star (List.map _normalise fs)
  | Septraction (f1, f2) -> Septraction (_normalise f1, _normalise f2)
  | atom -> atom

(** Perform normalisation until fixpoint is reached *)
let rec normalise phi =
  let phi' = _normalise phi in
  if equal phi' phi then phi'
  else normalise phi'

(** What exactly is the chunk size of single variable? *)
(** TODO: quantifiers *)
let rec chunk_size = function
  | Eq _ | Distinct _ | PointsTo _ | LS _ | DLS _ | SkipList _ | Var _ -> 1
  | Star psis -> BatList.sum @@ List.map chunk_size psis
  | Septraction (_, psi2) -> chunk_size psi2
  | And (psi1, psi2) | Or (psi1, psi2) | GuardedNeg (psi1, psi2) ->
      max (chunk_size psi1) (chunk_size psi2)
  | Not psi -> chunk_size psi

(** Fold using preorder traversal of AST *)
(** TODO: quantifiers *)
let rec fold (fn : t -> 'a -> 'a) phi (acc : 'a) = match node_type phi with
  | Var _ -> fn phi acc
  | Operator (terms, _) | Connective terms ->
      List.fold_left (fun acc term -> fold fn phi acc) acc terms

(** Assign ID to a subformula based on the DFS travelsal. *)

exception Match of int
exception NotSubformula

let rec subformula_id equal phi psi id =
  if equal phi psi then raise (Match id)
  else match node_type phi with
    | Var _ | Operator _ -> raise NotSubformula
    | Connective ts ->
      List.fold_left
        (fun id t -> try subformula_id equal t psi id with
          | NotSubformula -> id + 1
        ) (id + 1) ts
    | Quantifier (_, t) -> subformula_id equal t psi (id+1)

let subformula_id ?(physically=true) phi psi =
  let equal = if physically then (==) else (=) in
  try subformula_id equal phi psi 0 with
  | Match res -> res

(** Find subformula with given ID. *)
(** TODO: quantifiers *)
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
  | Quantifier _ ->
     begin match phi with
      | Exists (_, phi) -> is_negation_free phi
      | Forall _ -> false
     end

let rec is_symbolic_heap phi = match node_type phi with
  | Var _ | Operator _ -> true
  | _ ->
    begin match phi with
      | Star psis -> List.for_all is_symbolic_heap psis
      | Exists (_, psi) -> is_symbolic_heap psi
      | _ -> false
    end

let is_symbolic_heap_entl = function
  | GuardedNeg (psi1, psi2) -> is_symbolic_heap psi1 && is_symbolic_heap psi2
  | _ -> false

let rec is_positive phi = match node_type phi with
  | Var _ | Operator _ -> true
  | Connective terms ->
      begin match phi with
        | Not _ -> false
        | _ -> List.for_all is_positive terms
      end
  | Quantifier _ ->
     begin match phi with
      | Exists (_, phi) -> is_positive phi
      | Forall _ -> false
     end

let rec has_unique_shape phi = match phi with
  | Eq _ | Distinct _ | PointsTo _ -> true
  | And (psi1, psi2) -> has_unique_shape psi1 && has_unique_shape psi2
  | Star psis -> List.for_all has_unique_shape psis
  | _ -> false

let rec is_atomic phi = match node_type phi with
  | Var _ -> true
  | Operator _ ->
    begin match phi with
      | LS _ | DLS _ | SkipList _ -> false
      | _ -> true
    end
  | Connective terms -> List.for_all is_atomic terms
  | Quantifier _ -> false

let rec is_quantifier_free phi = match node_type phi with
  | Var _ | Operator _ -> true
  | Connective terms -> List.for_all is_quantifier_free terms
  | Quantifier _ -> false

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
  | Eq _ | Distinct _ | Pure _ | PointsTo _ | LS _ | DLS _ | SkipList _ -> true
  | And (f1, f2) -> has_unique_footprint f1 || has_unique_footprint f2
  | GuardedNeg (f1, f2) -> has_unique_footprint f1
  | Star psis -> List.for_all has_unique_footprint psis
  | Septraction (f1, f2) -> has_unique_footprint f1 && has_unique_footprint f2
  (* TODO: validate *)
  | Exists (_, psi) -> has_unique_footprint psi
  | Or _ | Not _ | Forall _ -> false

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

(** TODO: should this also handle free variables in pure terms? *)
let rec map_vars fn phi =
  let map_vars = map_vars fn in
  match phi with
  | Var x -> fn x
  | Pure t -> Pure t
  | Eq xs -> Eq (List.map map_vars xs)
  | Distinct xs -> Distinct (List.map map_vars xs)
  | PointsTo (x, ys) -> PointsTo (map_vars x, List.map map_vars ys)
  | LS (x, y) -> LS (map_vars x, map_vars y)
  | DLS (x, y, f, l) -> DLS (map_vars x, map_vars y, map_vars f, map_vars l)
  | SkipList (depth, x, y) -> SkipList (depth, map_vars x, map_vars y)
  | And (psi1, psi2) -> And (map_vars psi1, map_vars psi2)
  | Or (psi1, psi2) -> Or (map_vars psi1, map_vars psi2)
  | Not psi -> Not (map_vars psi)
  | GuardedNeg (psi1, psi2) -> GuardedNeg (map_vars psi1, map_vars psi2)
  | Exists (xs, psi) -> Exists (xs, map_vars psi)
  | Forall (xs, psi) -> Forall (xs, map_vars psi)
  | Star psis -> Star (List.map map_vars psis)
  | Septraction (psi1, psi2) -> Septraction (map_vars psi1, map_vars psi2)



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
  | Star fs -> List.iter iter fs; fn phi
  | Septraction (f1, f2) -> iter f1; iter f2; fn phi
  | Forall (_, psi) | Exists (_, psi) -> fn phi; iter psi
  | atom -> fn atom

let rec select_subformulae predicate phi =
  let acc = match node_type phi with
    | Var _ | Operator _ -> []
    | Connective psis -> List.concat @@ List.map (select_subformulae predicate) psis
    | Quantifier (_, psi) -> select_subformulae predicate psi
  in
  if predicate phi then phi :: acc else acc


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

module Infix = struct

  let (==)  = mk_eq
  let (!=)  = mk_distinct
  let (|->) = mk_pto
  let (|~>) = mk_ls

  let (=>)  = mk_implies
  let (<=>) = mk_bin_iff
  let (&!)  = mk_gneg
  let (&&)  = mk_bin_and
  let (||)  = mk_bin_or

  let ( * ) = mk_bin_star

end
