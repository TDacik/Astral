(* Internal representation of first-order formulae.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Logic_sig

module VariableBase = Variable.Make( )

module Range = struct

  type 'a t =
    | Range of 'a list list
    | Pair of ('a * 'a) list
    | Path of ('a * 'a * int) list

  let concat r1 r2 = match r1, r2 with
    | Some (Range xs), Some (Range ys) -> Some (Range (xs @ ys))
    | Some (Pair xs), Some (Pair ys) -> Some (Pair (xs @ ys))
    | Some (Path xs), Some (Path ys) -> Some (Path (xs @ ys))
    | None, None -> None

  let map (range : 'a t option) (fn : 'a -> 'b) = match range with
    | Some (Range xs) -> Some (Range (List.map (List.map fn) xs))
    | Some (Pair xs) -> Some (Pair (List.map (fun (x, y) -> fn x, fn y) xs))
    | Some (Path xs) -> Some (Path (List.map (fun (a, x, i) -> (fn a, fn x, i)) xs))
    | None -> None

  let lift_lists (r : 'a list list option) : 'a t option = Option.map (fun x -> Range x) r

end

module Term = struct

  type t =
    | Constant of String.t * Sort.t
    | Variable of VariableBase.t

    (* Propositional logic *)
    | And of t list
    | Or of t list
    | Not of t
    | Implies of t * t
    | Iff of t * t
    | IfThenElse of t * t * t
    | True
    | False

    (* Polymorphic operators *)
    | Equal of t list
    | Distinct of t list
    | LesserEq of t * t

    (* First-order quantifiers *)
    | Exists of t list * t Range.t option * t
    | Forall of t list * t Range.t option * t

    (* Second-order quantifiers *)
    | Exists2 of t list * t Range.t option * t
    | Forall2 of t list * t Range.t option * t

    (* Integer arithmetic *)
    | IntConst of int
    | Plus of t * t
    | Minus of t * t
    | Mult of t * t

    (* Bitvectors *)
    | BitConst of Bitvector.t
    | BitCheck of t * t
    | BitAnd of t list * Sort.t
    | BitOr of t list * Sort.t
    | BitXor of t list * Sort.t
    | BitImplies of t * t
    | BitCompl of t
    | BitShiftLeft of t * t    (* bitvector, integer *)
    | BitShiftRight of t * t   (* bitvector, integer *)

    (* Arrays *)
    | ConstArr of t * Sort.t   (* \lambda x : sort. t *)
    | Select of t * t
    | Store of t * t * t

    (* Sets *)
    | Membership of t * t
    | Subset of t * t
    | Disjoint of t list
    | Union of t list * Sort.t
    | Inter of t list * Sort.t
    | Diff of t * t
    | Compl of t
    | Enumeration of t list * Sort.t

    (* Sequences *)
    | Sequence of t list * Sort.t  (* Sequence constant *)
    | SeqIndex of t * t            (* Sequence indexing *)
    | SeqContains of t * t         (* Membership in sequence *)
    | SeqReverse of t              (* Reverse of sequence *)
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
      | Disjoint sets -> f (Disjoint (List.map map sets))
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

      | Sequence (seq, sort) -> f (Sequence (List.map map seq, sort))
      | SeqIndex (seq, index) -> f (SeqIndex (map seq, map index))
      | SeqContains (elem, seq) -> f (SeqContains (map elem, map seq))
      | SeqReverse seq -> f (SeqReverse (map seq))

      | Equal xs -> f (Equal (List.map map xs))
      | Distinct xs -> f (Distinct (List.map map xs))
      | And xs -> f (And (List.map map xs))
      | Or xs -> f (Or (List.map map xs))
      | Not x -> f (Not (map x))
      | Implies (x, y) -> f (Implies (map x, map y))
      | Iff (x, y) -> f (Iff (map x, map y))
      | IfThenElse (c, x, y) -> f (IfThenElse (map c, map x, map y))

      | LesserEq (x, y) -> f (LesserEq (map x, map y))

      (* TODO: apply also to ranges?? *)
      | Forall (xs, ranges, phi) -> f (Forall (xs, ranges, map phi))
      | Exists (xs, ranges, phi) -> f (Exists (xs, ranges, map phi))
      | Forall2 (xs, ranges, phi) -> f (Forall2 (xs, ranges, map phi))
      | Exists2 (xs, ranges, phi) -> f (Exists2 (xs, ranges, map phi))

  let rec describe_node = function
    | Constant (c, sort) -> (c, Operator ([], sort))
    | Variable (name, sort) -> (name, Var (name, sort))

    | IntConst i -> (string_of_int i, Operator ([], Sort.Int))
    | Plus (x, y) -> ("+", Operator ([x; y], Sort.Int))
    | Minus (x, y) -> ("-", Operator ([x; y], Sort.Int))
    | Mult (x, y) -> ("*", Operator ([x; y], Sort.Int))

    | Membership (elem, set) -> ("member", Connective [elem; set])
    | Subset (set1, set2) -> ("subset", Connective [set1; set2])
    | Disjoint sets -> ("disjoint", Connective sets)
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
    | Select (a, i) -> ("select", Operator ([a; i], Sort.get_dom_sort @@ get_sort a))

    | BitConst (n, width) ->
        (Format.asprintf "bitvector%d %d" width n, Operator ([], Sort.Bitvector width))
    | BitCheck (bv, index) -> ("bit-check", Operator ([bv; index], get_sort bv))
    | BitAnd (bvs, sort) -> ("bit-and", Operator (bvs, sort))
    | BitOr (bvs, sort) -> ("bit-or", Operator (bvs, sort))
    | BitXor (bvs, sort) -> ("bit-xor", Operator (bvs, sort))
    | BitImplies (bv1, bv2) -> ("bit-implies", Operator ([bv1; bv2], get_sort bv1))
    | BitCompl bv -> ("bit-complement", Operator ([bv], get_sort bv))
    | BitShiftLeft (bv, rot) -> ("<<", Operator ([bv; rot], get_sort bv))
    | BitShiftRight (bv, rot) -> (">>", Operator ([bv; rot], get_sort bv))

    | Sequence (seq, sort) -> ("seq TODO", Operator ([], sort))
    | SeqIndex (seq, index) -> ("seq.at", Operator ([seq; index], Sort.get_dom_sort @@ get_sort seq))
    | SeqContains (elem, seq) -> ("seq.contains", Connective ([elem; seq]))
    | SeqReverse seq -> ("seq.rev", Operator ([seq], get_sort seq))

    | Equal xs -> ("=", Connective xs)
    | Distinct xs -> ("distinct", Connective xs)
    | And xs -> ("and", Connective xs)
    | Or xs -> ("or", Connective xs)
    | Not x -> ("not", Connective [x])
    | Implies (x, y) -> ("=>", Connective [x; y])
    | Iff (x, y) -> ("<=>", Connective [x; y])
    | IfThenElse (c, x, y) -> ("ite", Operator ([c; x; y], get_sort x))
    | True -> ("true", Connective [])
    | False -> ("false", Connective [])

    (* Comparisons *)
    | LesserEq (x, y) -> ("<=", Connective [x; y])

    | Exists (xs, _, phi) -> ("exists", Quantifier (xs, phi))
    | Forall (xs, _, phi) -> ("forall", Quantifier (xs, phi))
    | Exists2 (xs, _, phi) -> ("exists2", Quantifier (xs, phi))
    | Forall2 (xs, _, phi) -> ("forall2", Quantifier (xs, phi))

  and node_name term = fst @@ describe_node term
  and node_type term = snd @@ describe_node term

  and get_sort t = match node_type t with
    | Var (_, sort) -> sort
    | Operator (_, sort) -> sort
    | Connective _ -> Sort.Bool
    | Quantifier _ -> Sort.Bool


  let pretty_select show term = (*`Node (fst @@ describe_node node)*)
    let rec compute index = function
      | Select (a, _) ->
        let count, index = compute a index in
        (count + 1, index)
      | _ -> 1, index
    in
    match term with
      | Select (a, i) ->
        let count, index = compute i a in
        (*`Tree*) (Format.asprintf "%s^%d[%s]" (show a) count (show index))

  let pretty_select show term = match term with
    | Select (a, i) -> pretty_select show term
    | other -> show other

  let pretty_print show node = None
  (*
    let str = match node with
      (*
      | Equal [x; y] -> `Tree (Format.asprintf "%s%s%s" (show x) (UnicodeSymbols.eq ()) (show y))
      *)
      | Equal _ -> `Node (UnicodeSymbols.eq ())
      | Distinct _ -> `Node (UnicodeSymbols.neq ())
      | And _ -> `Node (UnicodeSymbols.logand ())
      | Or _ -> `Node (UnicodeSymbols.logor ())
      | Not _ -> `Node (UnicodeSymbols.lognot ())
      | Exists _ -> `Node (UnicodeSymbols.exists ())
      | Forall _ -> `Node (UnicodeSymbols.forall ())
      | Select (arr, i) -> pretty_select show arr node
      | t -> `Node (fst @@ describe_node t)
    in
    Some str
  *)

  include Logic.Make
    (struct
      type nonrec t = t
      let describe_node = describe_node
      let pretty_print = pretty_print
    end)

  let get_sort_in_term var_name term =
    let vars = free_vars term in
    get_sort @@ List.find (fun v -> match v with Variable v -> String.equal (VariableBase.get_name v) var_name) vars

  let map_vars fn term =
    let fn = (fun t -> match node_type t with Var (name, sort) -> fn name sort | _ -> t) in
    map fn term

  (* TODO: more simplification *)
  let mk_eq_list ts =
    if List_utils.all_equal equal ts then True
    else Equal ts

  let mk_eq t1 t2 = mk_eq_list [t1; t2]

  (* TODO: more simplification *)
  let mk_distinct_list ts = Distinct ts

  let mk_distinct t1 t2 = mk_distinct_list [t1; t2]

  let compare_str t1 t2 = String.compare (show t1) (show t2)

  let to_smtlib_bench term =
    (free_vars term
     |> List.map (fun v -> Format.asprintf "(declare-fun %s %s)" (show v) (Sort.show @@ get_sort v))
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

  let of_term = function Variable (name, sort) -> (name, sort)

  let mk name sort = let name, sort = mk name sort in Variable (name, sort)
  let mk_fresh name sort = let name, sort = mk_fresh name sort in Variable (name, sort)

end

module Boolean = struct

  include Term

  let mk_var name = Variable.mk name Sort.Bool
  let mk_fresh_var name = Variable.mk_fresh name Sort.Bool

  let mk_false () = False
  let mk_true () = True

  let mk atom = if atom then mk_true () else mk_false ()

  let mk_const const = if const then mk_true () else mk_false ()

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
    let terms = List.filter (fun t -> not @@ equal t False) terms in
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

  let mk_ite c x y = match c with
    | True -> x
    | False -> y
    | term -> IfThenElse (term, x, y)

  let rec mk_multiple_ite cases t_else = match cases with
    | (c, t) :: rest -> mk_ite c t (mk_multiple_ite rest t_else)
    | [] -> t_else


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

module Arithmetic = struct

  include Term

  let mk_var name = Variable.mk name Sort.Int
  let mk_fresh_var name = Variable.mk_fresh name Sort.Int

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

  (** Sorts *)

  let mk_sort = Sort.mk_array

  let get_dom_sort arr = Sort.get_dom_sort @@ get_sort arr

  let get_range_sort arr = Sort.get_range_sort @@ get_sort arr

  (** Type checks *)

  let is_array term = Sort.is_array @@ get_sort term

  let is_elem_of elem arr = Sort.equal (get_sort elem) (get_range_sort arr)

  let is_index_of index arr = Sort.equal (get_sort index) (get_dom_sort arr)

  (** Basic constructors *)

  let mk_const t range_sort = ConstArr (t, range_sort)

  let mk_select arr index =
    (*
     * assert (is_array arr);
     * assert (is_index_of index arr);
     *)
    Select (arr, index)

  let mk_store arr index x =
    (*
     * assert (is_array arr);
     * assert (is_elem_of x arr);
     * assert (is_index_of index arr);
     *)
    Store (arr, index, x)

  (** Syntax sugar **)

  let mk_nary_select n arr x =
    List.init n (fun _ -> 0)
    |> List.fold_left (fun acc _ -> mk_select arr acc) x

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

  let mk_zero width = BitConst (Bitvector.zero width)
  let mk_one width = BitConst (Bitvector.one width)
  let mk_full_zeros width = BitConst (Bitvector.full_zeros width)
  let mk_full_ones width = BitConst (Bitvector.full_ones width)

  let mk_bit_check bv index = match bv, index with
    | BitConst bv, IntConst index -> if Bitvector.nth bv index then True else False
    | _ -> BitCheck (bv, index)

  let mk_and bvs (Sort.Bitvector n) = match bvs with
    | [] -> mk_full_ones n
    | [bv] -> bv
    | bvs -> BitAnd (bvs, Sort.Bitvector n)

  let mk_or bvs sort = match sort with
    | Sort.Bitvector n ->
    begin match bvs with
      | [] -> mk_full_zeros n
      | [bv] -> bv
      | bvs -> BitOr (bvs, sort)
    end
    | other -> Utils.internal_error ("Expected bitvector sort, got " ^ Sort.show other)

  let mk_xor bvs sort = BitXor (bvs, sort)
  let mk_implies bv1 bv2 = BitImplies (bv1, bv2)
  let mk_compl bv = BitCompl bv

  let mk_shift_left bv shift = BitShiftLeft (bv, shift)
  let mk_shift_right bv shift = BitShiftRight (bv, shift)

  let mk_lesser_eq bv1 bv2 = LesserEq (bv1, bv2)

  let to_bit_string (BitConst bv) = Bitvector.to_string bv

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

  let get_elem_sort set = Sort.get_dom_sort @@ Term.get_sort set

  let mk_sort elem_sort = Sort.Set elem_sort

  let mk_empty sort =
    assert (Sort.is_set sort);
    Enumeration ([], sort)

  let mk_singleton elem = Enumeration ([elem], Set (get_sort elem))

  let mk_mem elem set = Membership (elem, set)
  let mk_subset s1 s2 = Subset (s1, s2)

  let mk_disjoint_list = function
    | [] | [_] -> Boolean.mk_true ()
    | sets -> Disjoint sets

  let mk_disjoint s1 s2 = mk_disjoint_list [s1; s2]

  let mk_union_base sets sort =
    let sets = List.filter (fun s -> not @@ equal (mk_empty sort) s) sets in
    Union (sets, sort)

  let is_enum = function Enumeration _ -> true | _ -> false

  (* Accessors *)
  let get_elems = function
    | Enumeration (elems, _) -> elems
    | _ -> failwith "Not a constant set term"

  (** This is stronger that Term.is_const!! *)
  let rec simplify = function
    | Enumeration (sets, sort) -> Enumeration (List.map simplify sets, sort)
    | Union (sets, sort) ->
      let sets = List.map simplify sets in
      let consts, vars = List.partition is_enum sets in
      let consts = List.concat @@ List.map get_elems consts in
      begin match vars with
      | [] -> Enumeration (consts, sort)
      | xs -> Union (Enumeration (consts, sort) :: xs, sort)
      end
    | x -> x

  let mk_union sets sort =
    assert (Sort.is_set sort);
    let sets = List.map simplify sets in
    match sets with
    | [] -> mk_empty sort
    | [s] -> s
    | sets -> simplify (Union (sets, sort))

  let mk_inter sets sort =
    assert (Sort.is_set sort);
    match sets with
    | [s] -> s
    | sets -> Inter (sets, sort)

  let mk_diff t1 t2 = Diff (t1, t2)
  let mk_compl t = Compl t
  let mk_enumeration sort elements = Enumeration (elements, sort)

  let mk_add set elem = mk_union [set; mk_singleton elem] (get_sort set)

  let mk_eq_empty set = Term.mk_eq set (mk_empty @@ get_sort set)
  let mk_eq_singleton set x = Term.mk_eq set (mk_singleton x)

  (** Check whether two sets can be disjoint *)
  let may_disjoint sets =
    if List.for_all is_enum sets then
      let join =
        List.map get_elems sets
        |> List.concat
      in
      (List.length join) == (List.length @@ BatList.sort_unique compare join)
    else true

end

module Sequence = struct

  include Term

  let mk_sort domain_sort = Sort.Sequence domain_sort

  let mk_var = Variable.mk
  let mk_fresh_var = Variable.mk_fresh

  let get_elem_sort seq = Sort.get_dom_sort @@ Term.get_sort seq

  let mk_constant consts sort = Sequence (consts, sort)

  let mk_at_index seq index = SeqIndex (seq, index)

  let mk_contains seq index = SeqContains (seq, index)

  let mk_reverse seq = SeqReverse seq

end


    let rec substitute ?(bounded=[]) phi x term =
      let open Term in
      match phi with
      | Constant _ | BitConst _ -> phi
      (** TODO: Handling of sorts in comparison *)
      | Variable _ ->
          if equal x phi && not @@ List.mem x bounded
          then term
          else phi

      (* Quantifiers *)
      | Exists (binders, ranges, phi) ->
          Exists (binders, ranges, substitute ~bounded:(binders @ bounded) phi x term)
      | Forall (binders, ranges, phi) ->
          Forall (binders, ranges, substitute ~bounded:(binders @ bounded) phi x term)

      | Exists2 (binders, ranges, phi) ->
          Exists2 (binders, ranges, substitute ~bounded:(binders @ bounded) phi x term)
      | Forall2 (binders, ranges, phi) ->
          Forall2 (binders, ranges, substitute ~bounded:(binders @ bounded) phi x term)

      | Membership (elem, set) ->
        Membership (substitute ~bounded elem x term, substitute ~bounded set x term)
      | Subset (set1, set2) ->
        Subset (substitute ~bounded set1 x term, substitute ~bounded set2 x term)
      | Disjoint sets ->
        Disjoint (List.map (fun s -> substitute ~bounded s x term) sets)
      | Union (sets, sort) ->
        Set.mk_union (List.map (fun t -> substitute ~bounded t x term) sets) sort
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
      | Equal terms ->
        mk_eq_list (List.map (fun t -> substitute ~bounded t x term) terms)
      | Distinct terms ->
        mk_distinct_list (List.map (fun t -> substitute ~bounded t x term) terms)
      | And terms ->
        Boolean.mk_and (List.map (fun t -> substitute ~bounded t x term) terms)
      | Or terms ->
        Boolean.mk_or (List.map (fun t -> substitute ~bounded t x term) terms)
      | Not t ->
        Boolean.mk_not (substitute ~bounded t x term)
      | Implies (t1, t2) ->
        Implies (substitute ~bounded t1 x term, substitute ~bounded t2 x term)
      | Iff (t1, t2) ->
        Boolean.mk_iff (substitute ~bounded t1 x term) (substitute ~bounded t2 x term)
      | IfThenElse (c, t1, t2) ->
        IfThenElse (
          substitute ~bounded c x term,
          substitute ~bounded t1 x term,
          substitute ~bounded t2 x term
        )

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

  let substitute phi x term =
    (*let vars = Term.free_vars phi in
    if BatList.mem_cmp Term.compare x vars
    then*) substitute phi x term
    (*else phi*)

module Quantifier = struct

  module Range = Range

  let mk_forall xs ?(ranges=None) phi = Term.Forall (xs, Range.lift_lists ranges, phi)
  let mk_exists xs ?(ranges=None) phi = Term.Exists (xs, Range.lift_lists ranges, phi)

  let mk_forall_diagonal x1 x2 domain phi =
    let product = List_utils.diagonal_product domain in
    let range = Range.Pair product in
    Term.Forall ([x1; x2], Some range, phi)

  let mk_forall_path arr x max_length binder phi =
    (*let range = Range.Path [(arr, x, max_length)] in
    Term.Forall ([binder], Some range, phi)*)
    BatList.range 0 `To max_length
    |> List.map (fun i -> Array.mk_nary_select i arr x)
    |> (fun xs -> Term.Forall ([binder], Some (Range.Range [xs]), phi))


  let mk_forall_path_nested2 arr_top arr_next x (top, concrete, def) [x1; x2] phi =
    BatList.range 0 `To (snd top - 1)
    |> List.map (fun i -> Array.mk_nary_select i arr_top x, i)
    |> List.map (fun (loc, i) ->
        let bound =
          try List.nth concrete i
          with Failure _ -> def
        in
        BatList.range 0 `To (snd bound - 1)
        |> List.map (fun i -> Array.mk_nary_select i arr_next loc)
      )
    |> List.concat
    |> (fun xs ->
        let product = List_utils.diagonal_product xs in
        Term.Forall ([x1; x2], Some (Range.Pair product), phi))


  (** Preprocessing of quantifier binders. *)
  let process_quantifier xs ranges phi = match ranges with
    | None -> (xs, None, phi)
    | Some ranges ->
      let xs,  ranges, phi = List.fold_left2
        (fun (xs, ranges, phi) x range -> match range with
          | [] -> (xs, ranges, phi)
          | [r] -> (xs, ranges, substitute phi x r)
          | _ -> (x :: xs, range :: ranges, phi)
        ) ([], [], phi) xs ranges
      in
      (xs, Some ranges, phi)

  let mk_forall2 xs ?(ranges=None) phi =
    let xs, ranges, phi = process_quantifier xs ranges phi in
    match xs with
    | [] -> phi
    | _ -> Term.Forall2 (xs, Range.lift_lists ranges, phi)

  let mk_exists2 xs ?(ranges=None) phi =
    let xs, ranges, phi = process_quantifier xs ranges phi in
    match xs with
    | [] -> phi
    | _ -> Term.Exists2 (xs, Range.lift_lists ranges, phi)

end

module Model = struct

  open Term

  include Map.Make(Variable)

  (* Fix monomorphic type of the map *)
  type model = Term.t t

  let show model =
    bindings model
    |> List.map (fun (v, i) -> Format.asprintf "%s -> %s" (Variable.show v) (Term.show i))
    |> String.concat "\n"

  let show_with_sorts model =
    bindings model
    |> List.map
        (fun (v, i) -> Format.asprintf "%s -> %s" (Variable.show_with_sort v) (Term.show i))
    |> String.concat "\n"

  let rec eval model t = match t with
    | Constant (c, sort) -> Constant (c, sort)
    | Variable var ->
      begin
        try find var model
        with Not_found -> failwith ("Not found: " ^ Variable.show var)
      end

    | IntConst i -> IntConst i

    (* Bitvectors *)
    | BitConst bv -> BitConst bv

    (* Sets *)
    | Membership (x, set) ->
      let x = eval model x in
      begin match eval model set with
        | Enumeration (xs, _) -> Boolean.mk @@ BatList.mem_cmp Term.compare x xs
      end

    | Subset (s1, s2) -> failwith "TODO: eval set subset"
    | Disjoint sets -> failwith "TODO: eval disj"
    | Union (sets, _) -> failwith "TODO: eval union"
    | Inter (sets, _) -> failwith "TODO: eval inter"
    | Diff (s1, s2) -> failwith "TODO: eval diff"
    | Compl s -> failwith "TODO: eval compl"
    | Enumeration (_, _) -> t

    | Select (ConstArr (x, _), _) -> eval model x
    | Select (Store (a, i, v), j) ->
        let im = eval model i in
        let jm = eval model j in
        if Arithmetic.are_equal_consts i j
        then eval model v
        else eval model (Select (a, j))
    | Select (arr, i) -> eval model (Array.mk_select (eval model arr) i)

    | _ -> failwith ("TODO: eval other: " ^ Term.show t)

  let check model t = match eval model t with
    | True -> true
    | False -> false

  module Self = struct
    type t = model
    let show = show
  end

  include Datatype.Printable(Self)

end

include Term
