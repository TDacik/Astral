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

(* ==== Syntactic manipulation ==== *)

let rec substitute ?(bounded=[]) phi x term = match phi with
  | Variable (var, sort) ->
      if String.equal (match x with Variable (s, _) -> s) var then term
      else Variable (var, sort)

  (* Quantifiers *)
  | Exists (binder, phi) -> substitute ~bounded:(binder :: bounded) phi x term
  | Forall (binder, phi) -> substitute ~bounded:(binder :: bounded) phi x term

  (* TODO: others *)
  | _ -> phi

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

end

module Var = struct

  type t = var

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

  let size _ = 0 (* TODO *)

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

  let rec show = function
    | Constant (c, _) -> c
    | Variable (x, sort) -> Var.show (x, sort)

    | IntConst i -> Format.asprintf "%d" i
    | Plus (x, y) -> Format.asprintf "(%s + %s)" (show x) (show y)
    | Minus (x, y) -> Format.asprintf "(%s - %s)" (show x) (show y)
    | Mult (x, y) -> Format.asprintf "(%s * %s)" (show x) (show y)

    (* TODO: sets *)
    | Enumeration (enum, sort) ->
      begin match enum with
      | [] -> "∅"
      | s -> "{" ^ (List.map show s |> String.concat ",") ^ "}"
      end

    | ConstArr (const) -> Format.asprintf "(\\x. x = %s)" (show const)
    | Store (a, i, v) -> Format.asprintf "%s[%s <- %s]" (show a) (show i) (show v)
    | Select (a, i) -> Format.asprintf "%s[%s]" (show a) (show i)

    | Equal (x, y) -> Format.asprintf "(%s = %s)" (show x) (show y)
    | Distinct xs -> "(distinct " ^ (List.map show xs |> String.concat ",") ^ ")"
    | And xs -> "(and " ^ (List.map show xs |> String.concat ",") ^ ")"
    | Or xs -> "(or " ^ (List.map show xs |> String.concat ",") ^ ")"
    | Not x -> Format.asprintf "(not %s)" (show x)
    | Implies (x, y) -> Format.asprintf "(%s => %s)" (show x) (show y)
    | Iff (x, y) -> Format.asprintf "(%s <=> %s)" (show x) (show y)
    | True -> "true"
    | False -> "false"

    (* TODO: quantifiers *)
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

module Set = struct

  include Equality
  let get_elem_sort set = match get_sort set with
    | Set (elem_sort) -> elem_sort

  let mk_sort elem_sort = Set elem_sort

  let mk_empty sort = Enumeration ([], sort)

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
