(* Internal representation of first-order formulae
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

type var = String.t * sort

and sort =
  | Bool
  | Integer
  | Finite of String.t * string list
  | Set of sort
  | Array of sort * sort
  | Bitvector of sort * int (* Sort of indices *)

and term =
  | Constant of String.t * sort
  | Variable of var

  (* LIA *)
  | IntConst of int
  | Plus of term * term
  | Minus of term * term
  | Mult of term * term

  (* Sets *)
  | Membership of term * term
  | Subset of term * term
  | Disjoint of term * term
  | Union of term list * sort (* TODO: Is sort necessary? *)
  | Inter of term list * sort (* TODO: Is sort necessary? *)
  | Diff of term * term
  | Compl of term
  | Enumeration of term list * sort

  (* Arrays *)
  | ConstArr of term
  | Select of term * term
  | Store of term * term * term

  (* Bitvectors -- only minimal signature *)
  | BitConst of term * int * sort
  | BitIndex of term * term
  | BitAnd of term list * sort
  | BitOr of term list * sort
  | BitXor of term list * sort
  | BitImplies of term * term
  | BitCompl of term

  (* Boolean *)
  | Equal of term * term
  | Distinct of term list
  | And of term list
  | Or of term list
  | Not of term
  | Implies of term * term
  | Iff of term * term
  | True
  | False

  (* Quantifiers *)
  | Exists of term * term
  | Forall of term * term

module Sort = struct

  type t = sort

  let get_elem_sort = function Set (elem_sort) -> elem_sort
  let get_dom_sort = function Array (dom_sort, _) -> dom_sort
  let get_range_sort = function Array (_, range_sort) -> range_sort

  let rec show = function
    | Bool -> "boolean"
    | Integer -> "integer"
    | Finite (name, _) -> name
    | Set (elem_sort) -> Format.asprintf "(set %s)" (show elem_sort)
    | Array (dom, range) -> Format.asprintf "(array %s -> %s)" (show dom) (show range)
    | Bitvector (indices, width) -> Format.asprintf "(bitvector %d)" width

end

module Var = struct

  type t = var

  let get_name t = match t with
    | Variable (name, sort) -> name
    | _ -> failwith "not a variable"

  let compare (x1, _) (x2, _) = String.compare x1 x2

  let index = ref (-1)

  let mk name sort =
    if String.contains name ' '
    then Variable ("|" ^ name ^ "|", sort)
    else Variable (name, sort)

  let mk_fresh name sort =
    index := !index + 1;
    Variable (Format.asprintf "%s!%d" name !index, sort)

  let show (x, sort) = Format.asprintf "%s : %s" x (Sort.show sort)

end

module Term = struct

  type t = term

  let equal _ _ = true

  let compare _ _ = 0 (* TODO *)

  let rec get_sort = function
    | Constant (_, sort) -> sort
    | Variable (_, sort) -> sort

    (* Sets *)
    | Membership _ | Subset _ | Disjoint _ -> Bool
    | Union (_, sort) -> sort
    | Inter (_, sort) -> sort
    | Diff (s1, _) -> get_sort s1
    | Compl s -> get_sort s
    | Enumeration (_, sort) -> sort

    | ConstArr (const) -> failwith "TODO: sort of a constant array"
    | Select (a, _) -> Sort.get_range_sort @@ get_sort a
    | Store (a, _, _) -> get_sort a

    (* Bitvectors *)
    | BitConst (_, width, index_sort) -> Bitvector (index_sort, width)
    | BitIndex _ -> Bool
    | BitAnd (_, sort) -> sort
    | BitOr (_, sort) -> sort
    | BitXor (_, sort) -> sort
    | BitImplies (bv1, _) -> get_sort bv1
    | BitCompl bv -> get_sort bv

    | Equal _ -> Bool
    | Distinct _ -> Bool
    | And _ -> Bool
    | Or _ -> Bool
    | Not _ -> Bool
    | Implies _ -> Bool
    | Iff _ -> Bool
    | True -> Bool
    | False -> Bool

    (* Quantifiers *)
    | Exists _ | Forall _ -> Bool

  let rec map_vars fn term =
    let map_vars = map_vars fn in
    match term with
      | Constant (c, sort) -> Constant (c, sort)
      | Variable (x, sort) -> fn x sort
      | IntConst i -> IntConst i
      | Plus (x, y) -> Plus (map_vars x, map_vars y)
      | Minus (x, y) -> Minus (map_vars x, map_vars y)
      | Mult (x, y) -> Mult (map_vars x, map_vars y)

      | Membership (elem, set) -> Membership (map_vars elem, map_vars set)
      | Subset (set1, set2) -> Subset (map_vars set1, map_vars set2)
      | Disjoint (set1, set2) -> Disjoint (map_vars set1, map_vars set2)
      | Union (sets, sort) -> Union (List.map map_vars sets, sort)
      | Inter (sets, sort) -> Inter (List.map map_vars sets, sort)
      | Diff (set1, set2) -> Diff (map_vars set1, map_vars set2)
      | Compl set -> Compl (map_vars set)
      | Enumeration (enum, sort) -> Enumeration (List.map map_vars enum, sort)

      | ConstArr (const) -> ConstArr (map_vars const)
      | Store (a, i, v) -> Store (map_vars a, map_vars i, map_vars v)
      | Select (a, i) -> Select (map_vars a, map_vars i)

      | BitConst (bit, width, sort) -> BitConst (map_vars bit, width, sort)
      | BitIndex (bv, index) -> BitIndex (map_vars bv, map_vars index)
      | BitAnd (bvs, sort) -> BitAnd (List.map map_vars bvs, sort)
      | BitOr (bvs, sort) -> BitOr (List.map map_vars bvs, sort)
      | BitXor (bvs, sort) -> BitXor (List.map map_vars bvs, sort)
      | BitImplies (bv1, bv2) -> BitImplies (map_vars bv1, map_vars bv2)
      | BitCompl bv -> BitCompl (map_vars bv)

      | Equal (x, y) -> Equal (map_vars x, map_vars y)
      | Distinct xs -> Distinct (List.map map_vars xs)
      | And xs -> And (List.map map_vars xs)
      | Or xs -> Or (List.map map_vars xs)
      | Not x -> Not (map_vars x)
      | Implies (x, y) -> Implies (map_vars x, map_vars y)
      | Iff (x, y) -> Iff (map_vars x, map_vars y)
      | True -> True
      | False -> False


  let rec show = function
    | Constant (c, _) -> c
    | Variable (x, sort) -> Var.show (x, sort)

    | IntConst i -> Format.asprintf "%d" i
    | Plus (x, y) -> Format.asprintf "(%s + %s)" (show x) (show y)
    | Minus (x, y) -> Format.asprintf "(%s - %s)" (show x) (show y)
    | Mult (x, y) -> Format.asprintf "(%s * %s)" (show x) (show y)

    | Membership (elem, set) -> Format.asprintf "(member %s %s)" (show elem) (show set)
    | Subset (set1, set2) -> Format.asprintf "(subset %s %s)" (show set1) (show set2)
    | Disjoint (set1, set2) -> Format.asprintf "(disjoint %s %s)" (show set1) (show set2)
    | Union (sets, sort) -> "(union " ^ (List.map show sets |> String.concat ",") ^ ")"
    | Inter (sets, sort) -> "(inter " ^ (List.map show sets |> String.concat ",") ^ ")"
    | Diff (set1, set2) -> Format.asprintf "(minus %s %s)" (show set2) (show set2)
    | Compl set -> Format.asprintf "(complement %s)" (show set)
    | Enumeration (enum, sort) ->
      begin match enum with
      | [] -> "∅"
      | s -> "{" ^ (List.map show s |> String.concat ",") ^ "}"
      end

    | ConstArr (const) -> Format.asprintf "(\\x. x = %s)" (show const)
    | Store (a, i, v) -> Format.asprintf "%s[%s <- %s]" (show a) (show i) (show v)
    | Select (a, i) -> Format.asprintf "%s[%s]" (show a) (show i)

    | BitConst (bit, width, sort) -> Format.asprintf "[TODO] bitvectors"
    | BitIndex (bv, index) -> Format.asprintf "[TODO] bitvectors"
    | BitAnd (bvs, sort) -> Format.asprintf "[TODO] bitvectors"
    | BitOr (bvs, sort) -> Format.asprintf "[TODO] bitvectors"
    | BitXor (bv1, bv2) -> Format.asprintf "[TODO] bitvectors"
    | BitImplies (bv1, bv2) -> Format.asprintf "[TODO] bitvectors"
    | BitCompl bv -> Format.asprintf "[TODO] bitvecors"

    | Equal (x, y) -> Format.asprintf "(%s = %s)" (show x) (show y)
    | Distinct xs -> "(distinct " ^ (List.map show xs |> String.concat ",") ^ ")"
    | And xs -> "(and " ^ (List.map show xs |> String.concat ",") ^ ")"
    | Or xs -> "(or " ^ (List.map show xs |> String.concat ",") ^ ")"
    | Not x -> Format.asprintf "(not %s)" (show x)
    | Implies (x, y) -> Format.asprintf "(%s => %s)" (show x) (show y)
    | Iff (x, y) -> Format.asprintf "(%s <=> %s)" (show x) (show y)
    | True -> "true"
    | False -> "false"

  let rec size = function
    | Constant (c, _) -> 1
    | Variable (x, sort) -> 1

    | IntConst i -> 1
    | Plus (x, y) -> 1 + size x + size y
    | Minus (x, y) -> 1 + size x + size y
    | Mult (x, y) -> 1 + size x + size y

    | Membership (elem, set) -> 1 + size elem + size set
    | Subset (set1, set2) -> 1 + size set1 + size set2
    | Disjoint (set1, set2) -> 1 + size set1 + size set2
    | Union (sets, sort) -> List.fold_left (fun acc x -> acc + size x) 1 sets
    | Inter (sets, sort) -> List.fold_left (fun acc x -> acc + size x) 1 sets
    | Diff (set1, set2) -> 1 + size set1 + size set2
    | Compl set -> 1 + size set
    | Enumeration (enum, sort) -> List.fold_left (fun acc x -> acc + size x) 1 enum

    | ConstArr (const) -> 1
    | Store (a, i, v) -> 1 + size a + size i + size v
    | Select (a, i) -> 1 + size a + size i

    | BitConst (bit, width, sort) -> 1 + size bit
    | BitIndex (bv, index) -> 1 + size bv + size index
    | BitAnd (bvs, sort) -> 1 + List.fold_left (fun acc x -> acc + size x) 0 bvs
    | BitOr (bvs, sort) -> 1 + List.fold_left (fun acc x -> acc + size x) 0 bvs
    | BitXor (bvs, sort) -> 1 + List.fold_left (fun acc x -> acc + size x) 0 bvs
    | BitImplies (bv1, bv2) -> 1 + size bv1 + size bv2
    | BitCompl bv -> 1 + size bv

    | Equal (x, y) -> 1 + size x + size y
    | Distinct xs | And xs | Or xs -> List.fold_left (fun acc x -> acc + size x) 1 xs
    | Not x -> 1 + size x
    | Implies (x, y)
    | Iff (x, y) -> 1 + size x + size y
    | True -> 1
    | False -> 1
    | Exists (binder, phi) | Forall (binder, phi) -> 1 + size binder + size phi

    (* ==== Syntactic manipulation ==== *)

    let rec substitute ?(bounded=[]) phi x term = match phi with
      | Constant _ -> phi
      | Variable (var, sort) ->
          begin try
            if String.equal (match x with Variable (s, _) -> s) var then term
            else Variable (var, sort)
          with _ -> failwith (Format.asprintf "%s" (show x))
          end
      (* Quantifiers *)
      | Exists (binder, phi) -> substitute ~bounded:(binder :: bounded) phi x term
      | Forall (binder, phi) -> substitute ~bounded:(binder :: bounded) phi x term

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

    | IntConst i -> IntConst i
    | Plus (t1, t2) ->
        Plus (substitute ~bounded t1 x term, substitute ~bounded t2 x term)
    | Minus (t1, t2) ->
        Minus (substitute ~bounded t1 x term, substitute ~bounded t2 x term)
    | Mult (t1, t2) ->
        Mult (substitute ~bounded t1 x term, substitute ~bounded t2 x term)

  | ConstArr t ->
      ConstArr (substitute ~bounded t x term)
  | Select (arr, i) ->
      Select (substitute ~bounded arr x term, substitute ~bounded i x term)
  | Store (arr, i, v) ->
      Store (substitute ~bounded arr x term,
             substitute ~bounded i x term,
             substitute ~bounded v x term
            )

end

module Equality = struct

  let mk_eq t1 t2 = Equal (t1, t2)
  let mk_distinct ts = Distinct ts

  let mk_var = Var.mk
  let mk_fresh_var = Var.mk_fresh

  let get_sort = Term.get_sort
end

module Boolean = struct

  include Equality

  let mk_var name = mk_var name Bool

  let mk_false () = False
  let mk_true () = True
  let mk_and ts = And ts
  let mk_or ts = Or ts
  let mk_not t = Not t
  let mk_implies t1 t2 = Implies (t1, t2)
  let mk_iff t1 t2 = Iff (t1, t2)

end

module Enumeration = struct

  let mk_const sort name = Constant (name, sort)

  let mk_sort name constant_names = Finite (name, constant_names)
  let get_constants sort = match sort with
    | Finite (_, consts) -> List.map (mk_const sort) consts

end

module LIA = struct

  include Equality

  let mk_var name = mk_var name Integer

  let mk_const i = IntConst i
  let mk_plus t1 t2 = Plus (t1, t2)
  let mk_minus t1 t2 = Minus (t1, t2)
  let mk_mult t1 t2 = Mult (t1, t2)

  (* TODO: compare also sorts? *)
  let are_equal_consts t1 t2 = match t1, t2 with
  | (IntConst i, IntConst j) -> i = j
  | (Constant (x, _), Constant (y, _)) -> String.equal x y

end

module Array = struct

  include Equality

  let mk_sort dom range = Array (dom, range)

  let mk_const t = ConstArr t
  let mk_select arr x = Select (arr, x)

end

module Bitvector = struct

  include Equality

  let mk_sort width = Bitvector (Integer, width)

  let mk_custom_sort sort = match sort with
    | Finite (_, elements) -> Bitvector (sort, List.length elements)
    | _ -> failwith "Invalid argument"

  let mk_const c width sort = BitConst (c, width, sort)
  let mk_index bv i = BitIndex (bv, i)

  let mk_and bvs sort = BitAnd (bvs, sort)
  let mk_or bvs sort = BitOr (bvs, sort)
  let mk_xor bvs sort = BitXor (bvs, sort)
  let mk_implies bv1 bv2 = BitImplies (bv1, bv2)
  let mk_compl bv = BitCompl bv

end

module Set = struct

  include Equality
  let get_elem_sort set = match get_sort set with
    | Set (elem_sort) -> elem_sort

  let mk_sort elem_sort = Set elem_sort

  let mk_empty sort = Enumeration ([], sort)
  let mk_singleton elem = Enumeration ([elem], Set (get_sort elem))

  let mk_mem elem set = Membership (elem, set)
  let mk_subset s1 s2 = Subset (s1, s2)
  let mk_disjoint s1 s2 = Disjoint (s1, s2)

  let mk_union ts sort = Union (ts, sort)
  let mk_inter ts sort = Inter (ts, sort)
  let mk_diff t1 t2 = Diff (t1, t2)
  let mk_compl t = Compl t
  let mk_enumeration sort elements = Enumeration (elements, sort)

  let mk_eq_empty set = Equal (set, Enumeration ([], get_sort set))
  let mk_eq_singleton set x = Equal (set, Enumeration ([x], get_sort set))

  (* Accessors *)
  let get_elems = function Enumeration (elems, _) -> elems

  let rec simplify (term : Term.t) = match term with
    | Union (sets, sort) ->
        let sets = List.map simplify sets in
        begin try
          let xs = List.fold_left (fun acc set -> match set with
            | Enumeration (xs, _) -> acc @ xs
            | _ -> failwith ""
          ) [] sets
          in
          Enumeration (xs, sort)
        with _ -> term
        end
    | term -> term

  let may_disjoint set1 set2 = match simplify set1, simplify set2 with
    | Enumeration (e1, _), Enumeration (e2, _) ->
        not @@ List.exists (fun x -> List.mem x e2) e1
        && not @@ List.exists (fun x -> List.mem x e1) e2
    | _ -> true

end

module Quantifier = struct

  let mk_forall x phi = Forall (x, phi)
  let mk_exists x phi = Exists (x, phi)

end

module Model = struct

  include Map.Make(Var)

  (* Fix monomorphic type of the map *)
  type nonrec t = Term.t t

  let show model =
    bindings model
    |> List.map (fun (v, i) -> Format.asprintf "%s -> %s" (Var.show v) (Term.show i))
    |> String.concat "\n"

  let rec eval model t = match t with
    | Constant (c, sort) -> Constant (c, sort)
    | Variable (name, sort) -> find (name, sort) model

    | IntConst i -> IntConst i

    (* Sets *)
    | Membership (x, set) -> failwith "TODO: eval set mem"
    | Subset (s1, s2) -> failwith "TODO: eval set subset"
    | Disjoint (s1, s2) -> failwith "TODO: eval disj"
    | Union (sets, _) -> failwith "TODO: eval union"
    | Inter (sets, _) -> failwith "TDO: eval inter"
    | Diff (s1, s2) -> failwith "TODO: eval diff"
    | Compl s -> failwith "TODO: eval compl"
    | Enumeration (elems, _) -> failwith "TODO"

    | Select (Variable a, i) -> eval model (Array.mk_select (eval model (Variable a)) i)

    | Select (ConstArr x, _) -> eval model x
    | Select (Store (a, i, v), j) ->
        let im = eval model i in
        let jm = eval model j in
        if LIA.are_equal_consts i j
        then eval model v
        else eval model (Select (a, j))

    | _ -> failwith ("TODO: eval other: " ^ Term.show t)
end
