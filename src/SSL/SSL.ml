(* Syntax of strong-separation logic
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Logic_sig

module Variable = struct
  include Variable.Make( )

  let nil = mk "nil" Sort.loc_nil

  let is_loc var = match get_sort var with
    | Loc _ -> true
    | _ -> false

  let is_nil var = equal var nil

  let substitute var pattern target =
    if equal var pattern then target
    else var

  let hash = Hashtbl.hash

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end

module Field = struct

  type t =
    | Next
    | Prev
    | Top
    [@@deriving compare, equal]

  let show = function
    | Next -> "n"
    | Prev -> "p"
    | Top -> "t"

 module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end

module Struct = struct

  type t =
    | LS_t of Variable.t
    | DLS_t of Variable.t * Variable.t  (* next, prev *)
    | NLS_t of Variable.t * Variable.t  (* top, next *)
  [@@deriving compare, equal]

  let show = function
    | LS_t n -> Variable.show n
    | DLS_t (n, p) -> Format.asprintf "<n: %s, p: %s>" (Variable.show n) (Variable.show p)
    | NLS_t (t, n) -> Format.asprintf "<t: %s, n: %s>" (Variable.show t) (Variable.show n)

  let vars = function
    | LS_t n -> [n]
    | DLS_t (n, p) -> [n; p]
    | NLS_t (t, n) -> [t; n]

  let map fn = function
    | LS_t n -> LS_t (fn n)
    | DLS_t (n, p) -> DLS_t (fn n, fn p)
    | NLS_t (t, n) -> NLS_t (fn t, fn n)

  let substitute x pattern target = map (fun v -> Variable.substitute v pattern target) x

end


(** TODO: consider variadic and & or *)
type t =
  | Var of Variable.t     (* Location variable *)
  | Pure of SMT.Term.t    (* Pure boolean term which does not contain location variables *)

  (* Atoms *)
  | Emp
  | Eq of t list
  | Distinct of t list
  | PointsTo of t * Struct.t
  | LS of t * t
  | DLS of t * t * t * t
  | NLS of t * t * t
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
  | Var (name, sort) as v -> (name, Var (name, sort))
  | Pure t -> ("pure " ^ SMT.Term.show t, Operator ([], (SMT.Term.get_sort t)))
  | Eq xs -> ("=", Operator (xs, Sort.Bool))
  | Distinct xs -> ("distinct", Operator (xs, Sort.Bool))
  | Emp -> ("emp", Operator ([], Sort.Bool))
  | PointsTo (x, c) -> ("pto", Operator (x :: (List.map (fun v-> Var v) (Struct.vars c)), Sort.Bool))
  | LS (x, y) -> ("ls", Operator ([x; y], Sort.Bool))
  | DLS (x, y, f, l) -> ("dls", Operator ([x; y; f; l], Sort.Bool))
  | NLS (x, y, z) -> ("nls", Operator ([x; y; z], Sort.Bool))
  | SkipList (depth, x, y) -> (Format.asprintf "skl%d" depth, Operator ([x; y], Sort.Bool))
  | And (psi1, psi2) -> ("and", Connective [psi1; psi2])
  | Or (psi1, psi2) -> ("or", Connective [psi1; psi2])
  | Not psi -> ("not", Connective [psi])
  | GuardedNeg (psi1, psi2) -> ("gneg", Connective [psi1; psi2])
  | Exists (xs, psi) -> ("exists", Quantifier (xs, psi))
  | Forall (xs, psi) -> ("forall", Quantifier (xs, psi))
  | Star psis -> ("star", Connective psis)
  | Septraction (psi1, psi2) -> ("septraction", Connective [psi1; psi2])

let pretty_print show node =
  let str = match node with
    | Eq _ -> UnicodeSymbols.eq ()
    | Distinct _ -> UnicodeSymbols.neq ()
    | PointsTo _ -> UnicodeSymbols.pto ()
    | And _ -> UnicodeSymbols.logand ()
    | Or _ -> UnicodeSymbols.logor ()
    | Not _ -> UnicodeSymbols.lognot ()
    | GuardedNeg _ -> UnicodeSymbols.gneg ()
    | Star _ -> UnicodeSymbols.star ()
    | Septraction _ -> UnicodeSymbols.septraction ()
    | t -> fst @@ describe_node t
  in
  Some (`Node str)

(** First, build implementation of logical functions (including show). *)

module Self = struct
  type nonrec t = t
  let describe_node = describe_node
  let pretty_print = pretty_print
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

(** More precise version of equality intended for unit tests *)
(*  TODO: could perhaps use alpha-equivalence *)
let rec (===) lhs rhs =
  let rec fold = function
    | [] -> []
    | psis ->
      let stars, others = List.partition (function Star _ -> true | _ -> false) psis in
      let stars_operands = List.concat @@ List.map (function Star psis -> psis) stars in
      fold stars_operands @ others
  in
  match lhs, rhs with
  | Var v1, Var v2 -> Variable.equal v1 v2
  | Pure t1, Pure t2 -> SMT.Term.equal t1 t2
  | Eq xs1, Eq xs2 | Distinct xs1, Distinct xs2 ->
    (* Recursively uses weaker equality, but for variables this is fine *)
    Set.equal (Set.of_list xs1) (Set.of_list xs2)
  | Emp, Emp -> true
  | PointsTo (x1, y1), PointsTo (x2, y2) ->
    x1 === x2 && Struct.equal y1 y2
  | LS (x1, y1), LS (x2, y2) ->
    x1 === x2 && y1 === y2
  | NLS (x1, y1, z1), NLS (x2, y2, z2) ->
    x1 === x2 && y1 === y2 && z1 === z2
  | DLS (x1, y1, f1, l1), DLS (x2, y2, f2, l2) ->
    x1 === x2 && y1 === y2 && f1 === f2 && l1 === l2
  | And (lhs1, rhs1), And (lhs2, rhs2)
  | Or (lhs1, rhs1), Or (lhs2, rhs2) ->
    (lhs1 === lhs2 && rhs1 === rhs2) || (lhs1 === rhs2 && rhs1 === lhs2)
  | Not psi1, Not psi2 ->
    psi1 === psi2
  | Septraction (lhs1, rhs1), Septraction (lhs2, rhs2)
  | GuardedNeg (lhs1, rhs1), GuardedNeg (lhs2, rhs2) ->
    (lhs1 === lhs2 && rhs1 === rhs2)
  | Exists (xs1, psi1), Exists (xs2, psi2)
  | Forall (xs1, psi1), Forall (xs2, psi2) ->
    (* Recursively uses weaker equality, but for variables this is fine *)
    Set.equal (Set.of_list xs1) (Set.of_list xs2)
    && psi1 === psi2
  | Star psis1, Star psis2 ->
    let psis1 = List.sort compare (fold psis1) in
    let psis2 = List.sort compare (fold psis2) in
    List.equal (===) psis1 psis2
  | _, _ -> false


let is_var = function
  | Var _ -> true
  | _ -> false

let is_atom phi = match node_type phi with
  | Operator _ -> true
  | _ -> false

(* ==== Basic constructors ==== *)

let mk_emp () = Emp
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
let mk_var name sort = Var (Variable.mk name sort)
let mk_fresh_var name sort = Var (Variable.mk_fresh name sort)

let mk_eq x y = Eq [x; y]
let mk_eq_list xs = Eq xs

let mk_distinct x y = Distinct [x; y]
let mk_distinct_list xs = Distinct xs

let mk_pto_struct x y = PointsTo (x, y)

let mk_pto x (Var y) = PointsTo (x, Struct.LS_t y)
let mk_pto_dls x (Var n) (Var p) = PointsTo (x, Struct.DLS_t (n, p))
let mk_pto_nls x (Var t) (Var n) = PointsTo (x, Struct.NLS_t (t, n))

let mk_ls x y = LS (x, y)
let mk_dls x y f l = DLS (x, y, f, l)
let mk_nls x y z = NLS (x, y, z)
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
  | Emp | PointsTo _ | LS _ | DLS _ | NLS _ | SkipList _
  | Star _ | Septraction _ | Not _ -> false
  | _ ->
    begin match node_type phi with
    | Operator (terms, _) | Connective terms -> List.for_all is_pure terms
    | Quantifier (_, psi) -> is_pure psi
    end

(** Formula is pure_smt if it does not contain location variables. *)
let rec is_pure_smt phi = match node_type phi with
  | Var (_, _) -> false
  | Operator ([], _) -> false (*emp*)
  | Operator (terms, _) | Connective terms -> List.for_all is_pure_smt terms
  | Quantifier (_, psi) -> is_pure_smt psi

let rec substitute_pure phi x term = match phi with
  | Emp -> Emp
  | Var _ -> phi
  | Eq xs -> Eq xs
  | Distinct xs -> Distinct xs
  | PointsTo (x, y) -> PointsTo (x, y)
  | LS (x, y) -> LS (x, y)
  | DLS (x, y, f, l) -> DLS (x, y, f, l)
  | NLS (x, y, z) -> NLS (x, y, z)
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

let rec substitute ?(bounded=[]) phi v term = match phi with
  | Var _ ->
    if equal v phi && not @@ List.mem v bounded
    then term
    else phi
  | Emp -> Emp
  | Pure pure -> Pure pure
  | Eq xs -> Eq (List.map (fun x -> substitute ~bounded x v term) xs)
  | Distinct xs -> Eq (List.map (fun x -> substitute ~bounded x v term) xs)
  | PointsTo (x, d) ->
      PointsTo (substitute ~bounded x v term,
                Struct.map (fun y ->
                  let v = match v with Var v -> v in
                  let term = match term with Var v -> v in
                  if not @@ List.mem (Var y) bounded then Variable.substitute y v term
                  else y
                ) d
               )
  | LS (x, y) -> LS (substitute ~bounded x v term, substitute y v term)
  | DLS (x, y, f, l) ->
      DLS (substitute ~bounded x v term, substitute ~bounded y v term,
           substitute ~bounded f v term, substitute ~bounded l v term)
  | And (psi1, psi2) -> And (substitute ~bounded psi1 v term, substitute ~bounded psi2 v term)
  | Or (psi1, psi2) -> Or (substitute ~bounded psi1 v term, substitute ~bounded psi2 v term)
  | Not psi -> Not (substitute ~bounded psi v term)
  | GuardedNeg (psi1, psi2) ->
      GuardedNeg (substitute ~bounded psi1 v term, substitute ~bounded psi2 v term)
  | Exists (xs, psi) -> Exists (xs, substitute ~bounded:(bounded @ xs) psi v term)
  | Forall (xs, psi) -> Forall (xs, substitute ~bounded:(bounded @ xs) psi v term)
  | Star psis -> Star (List.map (fun psi -> substitute ~bounded psi v term) psis)
  | Septraction (psi1, psi2) ->
      Septraction (substitute ~bounded psi1 v term, substitute ~bounded psi2 v term)

let substitute phi v term = substitute ~bounded:[] phi v term

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

let get_root = function
  | PointsTo (Var x, _) | LS (Var x, _) | DLS (Var x, _, _, _)  | NLS (Var x, _, _) -> x

let get_sink = function
  | LS (_, Var y) | DLS (_, _, _, Var y)  | NLS (_, Var y, _) -> y

(** What exactly is the chunk size of single variable? *)
(** TODO: quantifiers *)
let rec chunk_size = function
  | Eq _ | Distinct _ | Emp | PointsTo _ | LS _ | DLS _ | NLS _ | SkipList _ | Var _ -> 1
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

let is_predicate phi = match phi with
  | LS _ | DLS _ | NLS _ -> true
  | _ -> false

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
  | Eq _ | Distinct _ | Emp | PointsTo _ -> true
  | And (psi1, psi2) -> has_unique_shape psi1 && has_unique_shape psi2
  | Star psis -> List.for_all has_unique_shape psis
  | _ -> false

let rec is_atomic phi = match node_type phi with
  | Var _ -> true
  | Operator _ -> is_positive phi && not @@ (is_predicate phi)
  | Connective terms -> is_positive phi && List.for_all is_atomic terms
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

(** {2 Views on SSL formulae} *)

type quantifier_view = [`Forall | `Exists] * Variable.t list * t

let as_quantifier = function
  | Forall (xs, phi) -> `Forall, List.map (fun (Var x) -> x) xs, phi
  | Exists (xs, phi) -> `Exists, List.map (fun (Var x) -> x) xs, phi

(* TODO: ... fix ... *)
type query =
  | QF_SymbolicHeap_SAT of t
  | QF_SymbolicHeap_ENTL of t * t
  | QF_Arbitrary_SAT of t
  | QF_Arbitrary_ENTL of t * t
  | SymbolicHeap_SAT of t
  | SymbolicHeap_ENTL of t * t list * t
  | Arbitrary_SAT of t
  | Arbitrary_ENTL of t * t

let as_query phi =
  if is_quantifier_free phi then begin
    if is_symbolic_heap phi then QF_SymbolicHeap_SAT phi
    else if is_symbolic_heap_entl phi then match phi with
      | GuardedNeg (lhs, rhs) -> QF_SymbolicHeap_ENTL (lhs, rhs)
    else match phi with
      | GuardedNeg (lhs, rhs) -> QF_Arbitrary_ENTL (lhs, rhs)
      | _ -> QF_Arbitrary_SAT phi
  end
  (* Quantified *)
  else if is_symbolic_heap_entl phi then match phi with
  | GuardedNeg (lhs, Exists (xs, rhs)) -> SymbolicHeap_ENTL (lhs, xs, rhs)
  else Arbitrary_SAT phi

let as_symbolic_heap phi =
  if not @@ is_symbolic_heap phi then failwith ("Not a symbolic heap:" ^ show phi)
  else match phi with
  | Star psis -> List.partition is_pure psis
  | psi when is_pure psi -> [psi], []
  | psi when is_atom psi -> [], [psi]

let rec has_unique_footprint = function
  | Eq _ | Distinct _ | Pure _ | Emp | PointsTo _ | LS _ | DLS _ | NLS _ | SkipList _ -> true
  | And (f1, f2) -> has_unique_footprint f1 || has_unique_footprint f2
  | GuardedNeg (f1, f2) -> has_unique_footprint f1
  | Star psis -> List.for_all has_unique_footprint psis
  | Septraction (f1, f2) -> has_unique_footprint f1 && has_unique_footprint f2
  | Or _ | Not _ | Exists _ | Forall _ -> false

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

let get_vars_sort sort phi =
  get_vars phi
  |> BatList.filter (Variable.has_sort sort)

let rec get_roots sort = function
  | Pure  _ | Emp | Eq _ | Distinct _ -> Variable.Set.empty
  | PointsTo (Var x, _) | LS (Var x, _) | NLS (Var x, _, _) -> Variable.Set.singleton x
  | DLS (Var x, Var x', _, _) -> Variable.Set.of_list [x; x']
  | And (psi1, psi2)
  | GuardedNeg (psi1, psi2)
  | Or (psi1, psi2) -> Variable.Set.union (get_roots sort psi1) (get_roots sort psi2)
  | Star psis ->
    List.fold_left
      (fun acc psi -> Variable.Set.union acc (get_roots sort psi))
      Variable.Set.empty psis

let get_roots sort phi = Variable.Set.elements @@ get_roots sort phi

let get_loc_sorts phi =
  get_vars phi
  |> List.map Variable.get_sort
  |> BatList.unique ~eq:Sort.equal

let rec map fn phi =
  let map = map fn in
  match phi with
  | Emp -> Emp
  | Var _ | Pure _ -> fn phi
  | Eq xs -> fn @@ Eq (List.map fn xs)
  | Distinct xs -> fn @@ Distinct (List.map fn xs)
  | PointsTo (x, d) -> fn @@ PointsTo (fn x, Struct.map (fun x -> match fn (Var x) with Var x -> x) d)
  | LS (x, y) -> fn @@ LS (fn x, fn y)
  | DLS (x, y, f, l) -> fn @@ DLS (fn x, fn y, fn f, fn l)
  | NLS (x, y, z) -> fn @@ NLS (fn x, fn y, fn z)
  | And (psi1, psi2) -> fn @@ And (map psi1, map psi2)
  | Or (psi1, psi2) -> fn @@ Or (map psi1, map psi2)
  | Not psi -> fn @@ Not (map psi)
  | GuardedNeg (psi1, psi2) -> fn @@ GuardedNeg (map psi1, map psi2)
  | Exists (xs, psi) -> fn @@ Exists (xs, map psi)
  | Forall (xs, psi) -> fn @@ Forall (xs, map psi)
  | Star psis -> fn @@ Star (List.map map psis)
  | Septraction (psi1, psi2) -> fn @@ Septraction (map psi1, map psi2)

let map_vars fn phi = map (function Var x -> fn x | psi -> psi) phi


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


let rename_var phi old_name new_name =
  map_vars
    (function (name, sort) ->
      if String.equal name old_name
      then Var (new_name, sort)
      else Var (name, sort)
    ) phi

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
