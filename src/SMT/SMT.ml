(* Internal representation of first-order formulae.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Logic_sig

module Sort = Sort
module VariableBase = Variable.Make( )

module Term = struct

  type term =
    | Constant of String.t * Sort.t
    | Variable of VariableBase.t

    (* LIA *)
    | IntConst of int
    | Plus of term * term
    | Minus of term * term
    | Mult of term * term

    (* Sets *)
    | Membership of term * term
    | Subset of term * term
    | Disjoint of term * term
    | Union of term list * Sort.t
    | Inter of term list * Sort.t
    | Diff of term * term
    | Compl of term
    | Enumeration of term list * Sort.t

    (* Arrays *)
    | ConstArr of term * Sort.t   (* \lambda x : sort. term *)
    | Select of term * term
    | Store of term * term * term

    (* Bitvectors *)
    | BitConst of Bitvector.t
    | BitCheck of term * term
    | BitAnd of term list * Sort.t
    | BitOr of term list * Sort.t
    | BitXor of term list * Sort.t
    | BitImplies of term * term
    | BitCompl of term
    | BitShiftLeft of term * term    (* bitvector, integer *)
    | BitShiftRight of term * term   (* bitvector, integer *)

    (* Boolean *)
    | Equal of term * term
    | Distinct of term list
    | And of term list
    | Or of term list
    | Not of term
    | Implies of term * term
    | Iff of term * term

    (* Polymorphic omparison *)
    | LesserEq of term * term

    | True
    | False

    (* First-order quantifiers *)
    | Exists of term list * term
    | Forall of term list * term

    (* Bounded second-order quantifiers *)
    | Exists2 of term list * term list list * term
    | Forall2 of term list * term list list * term

    [@@deriving map, fold]

  (* TODO: could this be derived? *)
  let rec map f term =
    let map = map f in
    match term with
      | Constant _ | Variable _ | IntConst _ | BitConst _ | True | False -> f term

      | Plus (x, y) -> f (Plus (map x, map y))
      | Minus (x, y) -> f (Minus (map x, map y))
      | Mult (x, y) -> f (Mult (map x, map y))

      | Membership (elem, set) -> f (Membership (map elem, map set))
      | Subset (set1, set2) -> f (Subset (map set1, map set2))
      | Disjoint (set1, set2) -> f (Disjoint (map set1, map set2))
      | Union (sets, sort) -> f (Union (List.map map sets, sort))
      | Inter (sets, sort) -> f (Inter (List.map map sets, sort))
      | Diff (set1, set2) -> f (Diff (map set1, map set2))
      | Compl set -> f (Compl (map set))
      | Enumeration (enum, sort) -> f (Enumeration (List.map map enum, sort))

      | BitCheck (bv, index) -> f (BitCheck (map bv, map index))
      | BitAnd (bvs, sort) -> f (BitAnd (List.map map bvs, sort))
      | BitOr (bvs, sort) -> f (BitOr (List.map map bvs, sort))
      | BitXor (bvs, sort) -> f (BitXor (List.map map bvs, sort))
      | BitImplies (bv1, bv2) -> f (BitImplies (map bv1, map bv2))
      | BitCompl bv -> f (BitCompl (map bv))
      | BitShiftLeft (bv, rot) -> f (BitShiftLeft (map bv, map rot))
      | BitShiftRight (bv, rot) -> f (BitShiftRight (map bv, map rot))

      | ConstArr (const, sort) -> f (ConstArr (map const, sort))
      | Store (a, i, v) -> f (Store (map a, map i, map v))
      | Select (a, i) -> f (Select (map a, map i))

      | Equal (x, y) -> f (Equal (map x, map y))
      | Distinct xs -> f (Distinct (List.map map xs))
      | And xs -> f (And (List.map map xs))
      | Or xs -> f (Or (List.map map xs))
      | Not x -> f (Not (map x))
      | Implies (x, y) -> f (Implies (map x, map y))
      | Iff (x, y) -> f (Iff (map x, map y))

      | LesserEq (x, y) -> f (LesserEq (map x, map y))

      | Forall (xs, phi) -> f (Forall (xs, map phi))
      | Exists (xs, phi) -> f (Exists (xs, map phi))
      | Forall2 (xs, ranges, phi) -> f (Forall2 (xs, ranges, map phi))
      | Exists2 (xs, ranges, phi) -> f (Exists2 (xs, ranges, map phi))

  let fold fn acc term = fold_term fn acc term

  let rec describe_node = function
    | Constant (c, sort) -> (c, Operator ([], sort))
    | Variable (name, sort) -> (name, Var (name, sort))

    | IntConst i -> (string_of_int i, Operator ([], Sort.Int))
    | Plus (x, y) -> ("+", Operator ([x; y], Sort.Int))
    | Minus (x, y) -> ("-", Operator ([x; y], Sort.Int))
    | Mult (x, y) -> ("*", Operator ([x; y], Sort.Int))

    | Membership (elem, set) -> ("member", Connective [elem; set])
    | Subset (set1, set2) -> ("subset", Connective [set1; set2])
    | Disjoint (set1, set2) -> ("disjoint", Connective [set1; set2])
    | Union (sets, sort) -> ("union", Operator (sets, sort))
    | Inter (sets, sort) -> ("inter", Operator (sets, sort))
    | Diff (set1, set2) -> ("minus", Operator ([set1; set2], get_sort set1))
    | Compl set -> ("complement", Operator ([set], get_sort set))
    | Enumeration (enum, sort) ->
        let name = match enum with
          | [] -> "empty"
          | _ -> "set"
        in
        (name, Operator (enum, sort))

    | ConstArr (const, sort) ->
        ("lambda x ->", Operator ([const], Sort.Array (get_sort const, sort)))
    | Store (a, i, v) -> ("store", Operator ([a; i; v], get_sort a))
    | Select (a, i) -> ("store", Operator ([a; i], get_sort a))

    | BitConst (n, width) ->
        (Format.asprintf "(bitvector%d %d)" width n, Operator ([], Sort.Bitvector width))
    | BitCheck (bv, index) -> ("bit-check", Operator ([bv; index], get_sort bv))
    | BitAnd (bvs, sort) -> ("bit-and", Operator (bvs, sort))
    | BitOr (bvs, sort) -> ("bit-or", Operator (bvs, sort))
    | BitXor (bvs, sort) -> ("bit-xor", Operator (bvs, sort))
    | BitImplies (bv1, bv2) -> ("bit-implies", Operator ([bv1; bv2], get_sort bv1))
    | BitCompl bv -> ("bit-complement", Operator ([bv], get_sort bv))
    | BitShiftLeft (bv, rot) -> ("<<", Operator ([bv; rot], get_sort bv))
    | BitShiftRight (bv, rot) -> (">>", Operator ([bv; rot], get_sort bv))

    | Equal (x, y) -> ("=", Connective [x; y])
    | Distinct xs -> ("distinct", Connective xs)
    | And xs -> ("and", Connective xs)
    | Or xs -> ("or", Connective xs)
    | Not x -> ("not", Connective [x])
    | Implies (x, y) -> ("=>", Connective [x; y])
    | Iff (x, y) -> ("<=>", Connective [x; y])
    | True -> ("true", Connective [])
    | False -> ("false", Connective [])

    (* Comparisons *)
    | LesserEq (x, y) -> ("<=", Connective [x; y])

    | Exists (xs, phi) -> ("exists", Quantifier (xs, phi))
    | Forall (xs, phi) -> ("forall", Quantifier (xs, phi))
    | Exists2 (xs, _, phi) -> ("exists2", Quantifier (xs, phi))
    | Forall2 (xs, _, phi) -> ("forall2", Quantifier (xs, phi))

  and node_name term = fst @@ describe_node term
  and node_type term = snd @@ describe_node term

  and get_sort t = match node_type t with
    | Var (_, sort) -> sort
    | Operator (_, sort) -> sort
    | Connective _ -> Sort.Bool
    | Quantifier _ -> Sort.Bool

  type t = term

  include Logic.Make
    (struct
      type t = term
      let describe_node = describe_node
    end)

  let map_vars fn term =
    let fn = (fun t -> match node_type t with Var (name, sort) -> fn name sort | _ -> t) in
    map fn term

  (* TODO:
      - sorts
      - sorting of operands (especially for enums)
   *)
  let rec identity t1 t2 = match describe_node t1, describe_node t2 with
    | (_, Var (n1, s1)), (_, Var (n2, s2)) -> VariableBase.equal (n1, s1) (n2, s2)
    | (name1, Operator (terms1, _)), (name2, Operator (terms2, _))
    | (name1, Connective terms1), (name2, Connective terms2) ->
        begin
          try String.equal name1 name2
              && List.for_all2 identity terms1 terms2
          with Invalid_argument _ -> false
        end
    | (name1, Quantifier (xs1, phi1)), (name2, Quantifier (xs2, phi2)) ->
        begin
          try String.equal name1 name2
              && List.for_all2 identity xs1 xs2
              && identity phi1 phi2
          with Invalid_argument _ -> false
        end
    | _ -> false

  (** Equality on terms is defined using their identity *)
  let equal = identity

  let compare t1 t2 = if equal t1 t2 then 0 else Stdlib.compare t1 t2

  let mk_eq t1 t2 =
    if identity t1 t2 then True
    else Equal (t1, t2)

  let mk_neq t1 t2 = Distinct [t1; t2]

  let mk_distinct ts = Distinct ts

  let compare_str t1 t2 = String.compare (show t1) (show t2)

  let to_bench term =
    (free_vars term
     |> List.map (fun v -> Format.asprintf "(declare-fun %s (%s))" (show v) (Sort.show @@ get_sort v))
     |> String.concat "\n"
    )
    ^ "\n\n" ^
    (show term)

  (*
  let rec substitute ?(bounded=[]) phi x term =
    let fn =
      (fun t -> match node_type t with
        | Var (name1, _) ->
            let name2 = VariableBase.get_name x in
            if String.equal name1 name2 && not @@ List.mem name1 bounded
            then term
            else phi (* ???? *)
        | Operator terms | Connective terms ->
            List.map (substitute ~bounded phi x) terms
        | Quantifier (binders, phi) ->
            (binders, substitute ~bounded:(binders @ bounded) phi x term)
      )
    in
    map fn term
  let rec substitute ?(bounded=[]) phi x term = match node_type phi with
    | Var ->*)

    (* ==== Syntactic manipulation ==== *)

    let rec substitute ?(bounded=[]) phi x term = match phi with
      | Constant _ | BitConst _ -> phi
      (** TODO: Handling of sorts in comparison *)
      | Variable _ ->
          if equal x phi && not @@ List.mem x bounded
          then term
          else phi

      (* Quantifiers *)
      | Exists (binders, phi) ->
          Exists (binders, substitute ~bounded:(binders @ bounded) phi x term)
      | Forall (binders, phi) ->
          Forall (binders, substitute ~bounded:(binders @ bounded) phi x term)

      | Exists2 (binders, ranges, phi) ->
          Exists2 (binders, ranges, substitute ~bounded:(binders @ bounded) phi x term)
      | Forall2 (binders, ranges, phi) ->
          Forall2 (binders, ranges, substitute ~bounded:(binders @ bounded) phi x term)

      | Membership (elem, set) ->
        Membership (substitute ~bounded elem x term, substitute ~bounded set x term)
      | Subset (set1, set2) ->
        Subset (substitute ~bounded set1 x term, substitute ~bounded set2 x term)
      | Disjoint (set1, set2) ->
        Disjoint (substitute ~bounded set1 x term, substitute ~bounded set2 x term)
      | Union (sets, sort) ->
        Union (List.map (fun t -> substitute ~bounded t x term) sets, sort)
      | Inter (sets, sort) ->
        Inter (List.map (fun t -> substitute ~bounded t x term) sets, sort)
      | Diff (set1, set2) ->
        Diff (substitute ~bounded set1 x term, substitute ~bounded set2 x term)
      | Compl set ->
        Compl (substitute ~bounded set x term)
      | Enumeration (terms, sort) ->
        Enumeration (List.map (fun t -> substitute ~bounded t x term) terms, sort)

      (* Bitvectors *)
      | BitCheck (bv, index) ->
        BitCheck (substitute ~bounded bv x term, substitute ~bounded index x term)
      | BitAnd (bvs, sort) ->
        BitAnd (List.map (fun t -> substitute ~bounded t x term) bvs, sort)
      | BitOr (bvs, sort) ->
        BitOr (List.map (fun t -> substitute ~bounded t x term) bvs, sort)
      | BitXor (bvs, sort) ->
        BitXor (List.map (fun t -> substitute ~bounded t x term) bvs, sort)
      | BitImplies (bv1, bv2) ->
        BitImplies (substitute ~bounded bv1 x term, substitute ~bounded bv2 x term)
      | BitCompl bv ->
        BitCompl (substitute ~bounded bv x term)
      | BitShiftLeft (bv, rot) ->
        BitShiftLeft (substitute ~bounded bv x term, substitute ~bounded rot x term)
      | BitShiftRight (bv, rot) ->
        BitShiftRight (substitute ~bounded bv x term, substitute ~bounded rot x term)

      (* Boolean *)
      | Equal (t1, t2) ->
        Equal (substitute ~bounded t1 x term, substitute ~bounded t2 x term)
      | Distinct terms ->
        Distinct (List.map (fun t -> substitute ~bounded t x term) terms)
      | And terms ->
        And (List.map (fun t -> substitute ~bounded t x term) terms)
      | Or terms ->
        Or (List.map (fun t -> substitute ~bounded t x term) terms)
      | Not t ->
        Not (substitute ~bounded t x term)
      | Implies (t1, t2) ->
        Implies (substitute ~bounded t1 x term, substitute ~bounded t2 x term)
      | Iff (t1, t2) ->
        Iff (substitute ~bounded t1 x term, substitute ~bounded t2 x term)
      | True -> True
      | False -> False

      | LesserEq (t1, t2) ->
        LesserEq (substitute ~bounded t1 x term, substitute ~bounded t2 x term)

      | IntConst i -> IntConst i
      | Plus (t1, t2) ->
        Plus (substitute ~bounded t1 x term, substitute ~bounded t2 x term)
      | Minus (t1, t2) ->
        Minus (substitute ~bounded t1 x term, substitute ~bounded t2 x term)
      | Mult (t1, t2) ->
        Mult (substitute ~bounded t1 x term, substitute ~bounded t2 x term)

      | ConstArr (t, sort) ->
        ConstArr (substitute ~bounded t x term, sort)
      | Select (arr, i) ->
        Select (substitute ~bounded arr x term, substitute ~bounded i x term)
      | Store (arr, i, v) ->
        Store (substitute ~bounded arr x term,
          substitute ~bounded i x term,
          substitute ~bounded v x term
        )

  module Self = struct
    type nonrec t = t
    let compare = compare
    let show = show
  end

  include Datatype.Printable(Self)

end

module Variable = struct

  include Term
  include VariableBase

  let mk name sort = let name, sort = mk name sort in Variable (name, sort)
  let mk_fresh name sort = let name, sort = mk_fresh name sort in Variable (name, sort)

end

module Boolean = struct

  include Term

  let mk_var name = Variable.mk name Sort.Bool

  let mk_false () = False
  let mk_true () = True

  let mk_and terms =
    let terms = List.fold_left
      (fun acc t -> match t with
        | True -> acc
        | And ts -> ts @ acc
        | t -> t :: acc
      ) [] terms
    in match terms with
      | [] -> True
      | [t] -> t
      | terms -> And terms

  let mk_or terms =
    let terms = List.filter (fun t -> not @@ identity t False) terms in
    match terms with
    | [] -> False
    | [t] -> t
    | terms -> Or terms

  let mk_not = function
    | True -> False
    | False -> True
    | term -> Not term

  let mk_implies t1 t2 = Implies (t1, t2)
  let mk_iff t1 t2 = Iff (t1, t2)

end

module Enumeration = struct

  include Term

  let mk_var name sort = Variable.mk name sort
  let mk_fresh_var name sort = Variable.mk_fresh name sort

  let mk_const sort name = Constant (name, sort)

  let mk_sort name constant_names = Sort.Finite (name, constant_names)

  let get_constants sort = match sort with
    | Sort.Finite (_, consts) -> List.map (mk_const sort) consts

  let cardinality sort = match sort with
    | Sort.Finite (_, consts) -> List.length consts

end

module LIA = struct

  include Term

  let mk_var name = Variable.mk name Sort.Int

  let mk_sort = Sort.Int

  let mk_const i = IntConst i
  let mk_plus t1 t2 = Plus (t1, t2)
  let mk_minus t1 t2 = Minus (t1, t2)
  let mk_mult t1 t2 = Mult (t1, t2)

  let mk_lesser_eq t1 t2 = LesserEq (t1, t2)

  (* TODO: compare also sorts? *)
  let are_equal_consts t1 t2 = match t1, t2 with
  | IntConst i, IntConst j -> i = j
  | Constant (x, _), Constant (y, _) -> String.equal x y

  (* TODO: compare also widths?? *)
  | BitConst (x, _), BitConst (y, _) -> Int.equal x y

end

module Array = struct

  include Term

  let mk_var = Variable.mk
  let mk_fresh_var = Variable.mk_fresh

  let mk_sort dom range = Sort.Array (dom, range)

  let mk_const t range_sort = ConstArr (t, range_sort)
  let mk_select arr x = Select (arr, x)

end

module Bitvector = struct

  include Term

  let mk_sort width =
    if width > 0 then Sort.Bitvector width
    else failwith "Bitvector width must be greater than zero"

  let mk_var name sort = Variable.mk name sort
  let mk_fresh_var name sort = Variable.mk_fresh name sort

  let get_width bv = match get_sort bv with
    | Sort.Bitvector n -> n
    | _ -> failwith "Argument is not a bitvector"

  let mk_const n width = BitConst (Bitvector.of_int n width)
  let mk_const_of_string str = BitConst (Bitvector.of_string str)

  let mk_index bv i = BitCheck (bv, i)

  let mk_zero width = BitConst (Bitvector.zero width)
  let mk_one width = BitConst (Bitvector.one width)
  let mk_full_zeros width = BitConst (Bitvector.full_zeros width)
  let mk_full_ones width = BitConst (Bitvector.full_ones width)

  let mk_and bvs (Sort.Bitvector n) = match bvs with
    | [] -> mk_full_ones n
    | [bv] -> bv
    | bvs -> BitAnd (bvs, Sort.Bitvector n)

  let mk_or bvs (Sort.Bitvector n) = match bvs with
    | [] -> mk_full_zeros n
    | [bv] -> bv
    | bvs -> BitOr (bvs, Sort.Bitvector n)

  let mk_xor bvs sort = BitXor (bvs, sort)
  let mk_implies bv1 bv2 = BitImplies (bv1, bv2)
  let mk_compl bv = BitCompl bv

  let mk_shift_left bv shift = BitShiftLeft (bv, shift)
  let mk_shift_right bv shift = BitShiftRight (bv, shift)

  let mk_lesser_eq bv1 bv2 = LesserEq (bv1, bv2)

end

(** Utility for smart constructors *)
let construct cons (f : Term.t list -> Term.t list -> Term.t list) (acc : Term.t list) terms sort =
  let consts, vars = List.partition Term.is_constant terms in
  let folded = List.fold_left
    (fun acc x -> match x with
      | Term.Enumeration (cs, _) -> f cs acc
    ) acc consts
  in
  match vars with
  | [] -> Term.Enumeration (List.sort Stdlib.compare folded, sort)
  | cs -> cons ((Term.Enumeration (folded, sort)) :: cs) sort

module Set = struct

  include Term

  let mk_var = Variable.mk
  let mk_fresh_var = Variable.mk_fresh

  let get_elem_sort set = Sort.get_elem_sort @@ Term.get_sort set

  let mk_sort elem_sort = Sort.Set elem_sort

  let mk_empty sort = Enumeration ([], sort)
  let mk_singleton elem = Enumeration ([elem], Set (get_sort elem))

  let mk_mem elem set = Membership (elem, set)
  let mk_subset s1 s2 = Subset (s1, s2)
  let mk_disjoint s1 s2 = Disjoint (s1, s2)

  let mk_union ts sort = construct (fun es sort -> Union (es, sort)) (@) [] ts sort
  let mk_inter ts sort = Inter (ts, sort)
  let mk_diff t1 t2 = Diff (t1, t2)
  let mk_compl t = Compl t
  let mk_enumeration sort elements = Enumeration (elements, sort)

  let mk_eq_empty set = Equal (set, Enumeration ([], get_sort set))
  let mk_eq_singleton set x = Equal (set, Enumeration ([x], get_sort set))

  (* Accessors *)
  let get_elems = function
    | Enumeration (elems, _) -> elems
    | _ -> failwith "Not a constant set term"

  (** Check whether two sets can be disjoint *)
  let may_disjoint set1 set2 = match set1, set2 with
    | Enumeration (e1, _), Enumeration (e2, _) ->
        not @@ List.exists (fun x -> List.mem x e2) e1
        && not @@ List.exists (fun x -> List.mem x e1) e2
    | _ -> true

end

module Quantifier = struct

  let mk_forall x phi = Term.Forall (x, phi)
  let mk_exists x phi = Term.Exists (x, phi)

  (** Preprocessing of quantifier binders. *)
  let process_quantifier xs ranges phi =
    List.fold_left2
      (fun (xs, ranges, phi) x range -> match range with
        (*| [] -> (xs, ranges, phi)
        | [r] -> (xs, ranges, Term.substitute phi x r)*)
        | _ -> (x :: xs, range :: ranges, phi)
      ) ([], [], phi) xs ranges

  let mk_forall2 xs ranges phi =
    let xs, ranges, phi = process_quantifier xs ranges phi in
    match xs with
    | [] -> phi
    | _ -> Term.Forall2 (xs, ranges, phi)

  let mk_exists2 xs ranges phi =
    let xs, ranges, phi = process_quantifier xs ranges phi in
    match xs with
    | [] -> phi
    | _ -> Term.Exists2 (xs, ranges, phi)

end

module Model = struct

  open Term

  include Map.Make(Variable)

  (* Fix monomorphic type of the map *)
  type nonrec t = Term.t t

  let show model =
    bindings model
    |> List.map (fun (v, i) -> Format.asprintf "%s -> %s" (Variable.show v) (Term.show i))
    |> String.concat "\n"

  let rec eval model t = match t with
    | Constant (c, sort) -> Constant (c, sort)
    | Variable var -> find var model

    | IntConst i -> IntConst i

    (* Bitvectors *)
    | BitConst bv -> BitConst bv

    (* Sets *)
    | Membership (x, set) -> failwith "TODO: eval set mem"
    | Subset (s1, s2) -> failwith "TODO: eval set subset"
    | Disjoint (s1, s2) -> failwith "TODO: eval disj"
    | Union (sets, _) -> failwith "TODO: eval union"
    | Inter (sets, _) -> failwith "TODO: eval inter"
    | Diff (s1, s2) -> failwith "TODO: eval diff"
    | Compl s -> failwith "TODO: eval compl"
    | Enumeration (elems, _) -> t

    | Select (ConstArr (x, _), _) -> eval model x
    | Select (Store (a, i, v), j) ->
        let im = eval model i in
        let jm = eval model j in
        if LIA.are_equal_consts i j
        then eval model v
        else eval model (Select (a, j))
    | Select (arr, i) -> eval model (Array.mk_select (eval model arr) i)

    | _ -> failwith ("TODO: eval other: " ^ Term.show t)
end

include Term

(* ==== Tests ==== *)

let (===) = identity
let (!===) x y = not @@ identity x y

let sort = Sort.Int

let x = LIA.mk_var "x"
let y = LIA.mk_var "y"
let z = LIA.mk_var "z"

let c1 = LIA.mk_const 1
let c2 = LIA.mk_const 2
let c3 = LIA.mk_const 3

let%test _ = map_vars (fun x sort -> Variable (x, sort)) True === True
let%test _ = map_vars (fun x sort -> Variable (x, sort))
              (Variable ("x", Sort.Int)) === (Variable ("x!0", Sort.Int))

let%test "map1" =
  let fn = (fun t -> match t with Forall (_, phi) -> phi | _ -> t) in
  Printf.printf "Term: %s\n" (show @@ map fn (Quantifier.mk_forall [x] x));
  map fn (Quantifier.mk_forall [x] x) === x

let%test "substite_var" =
  substitute (LIA.mk_var "x") (LIA.mk_var "x") (LIA.mk_var "y") === (LIA.mk_var "y")

let%test "substitute_bounded" =
  substitute
    (Quantifier.mk_forall [(LIA.mk_var "x")] (LIA.mk_var "x"))
    (LIA.mk_var "x")
    (LIA.mk_var "y")
  === (Quantifier.mk_forall [(LIA.mk_var "x")] (LIA.mk_var "x"))

let%test "substitute_free" =
  substitute
    (Quantifier.mk_forall [(LIA.mk_var "x")] (LIA.mk_var "y"))
    (LIA.mk_var "y")
    (LIA.mk_var "z")
  === (Quantifier.mk_forall [(LIA.mk_var "x")] (LIA.mk_var "z"))

let%test "size_var" = size (LIA.mk_var "x") == 1
let%test "size_term" = size (LIA.mk_plus (LIA.mk_const 3) (LIA.mk_const 2)) == 3
let%test "size_quantifier" = size (Quantifier.mk_forall [x; y] (LIA.mk_lesser_eq x y)) == 5

let%test "and_cons1" = (Boolean.mk_and []) === True
let%test "and_cons2" = (Boolean.mk_and [True; True]) === True
let%test "and_cons3" = (Boolean.mk_and [True; False]) === False

let%test "set_cons1" =
  (Set.mk_union [(Set.mk_enumeration sort [c1; c2]); (Set.mk_singleton c3)] sort)
  === (Set.mk_enumeration sort [c1; c2; c3])
