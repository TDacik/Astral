(* This module serves as a base implementation for both first-order and separation logic.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

[@@@warning "+8"]

open MemoryModel

module Variable = Variable.Make()
module Sort = Sort

module Application = struct

  type t =
    | Constant of Constant.t
    (* Equality *)
    | Equal | Distinct
    (* Propositional Logic *)
    | And | Or | Not | Implies | Iff | IfThenElse
    (* Arithmetic *)
    | Plus | Minus | Mult | Lesser | LesserEqual
    (* Bitvectors *)
    | BitPlus of Int.t
    | BitCheck | BitNot | BitAnd of Int.t | BitOr of Int.t | BitXor of Int.t
    | BitUnsignedLesser | BitUnsignedLesserEqual
    | BitImplies | BitCompl | BitShiftLeft | BitShiftRight
    (* Sets *)
    | Membership | Subset | Disjoint | Union of Sort.t | Inter of Sort.t | Diff | Compl
    | Enum of Sort.t | Universe of Sort.t
    (* Arrays *)
    | ConstArray of Sort.t | Select | Store
    (* Separation logic *)
    | Emp | Pure | PointsTo of StructDef.t
    | Predicate of Identifier.t * (StructDef.t List.t)
    | HeapTerm of MemoryModel0.Field.t | BlockBegin | BlockEnd
    | GuardedNot | Star | Septraction
  [@@ deriving equal, compare]

  (** TODO: So far only used for tests. Proper implementation can be used
            for comparison. *)
  let can_reorder = function
    | Constant _ | Equal | And | Or | Iff | Star | Union _ | Inter _ -> true
    | _ -> false

  let show = function
    | Constant c -> Constant.show c
    | Equal -> "eq" | Distinct -> "distinct"
    | And -> "and" | Or -> "or" | Not -> "not" | Implies -> "->"
    | Iff -> "<->" | IfThenElse -> "ite"

    | Lesser | BitUnsignedLesser -> "<"
    | LesserEqual | BitUnsignedLesserEqual -> "<="

    | Plus -> "+" | Minus -> "-" | Mult -> "*"
    | BitPlus _ -> "bvadd"
    | BitCheck -> "bit-check"
    | BitNot -> "bit-not"
    | BitAnd _ -> "bit-and"
    | BitOr _ -> "bit-or"
    | BitXor _ -> "bit-xor"
    | BitImplies -> "bit-implies" | BitCompl -> "bit-compl"
    | BitShiftLeft -> ">>" | BitShiftRight -> "<<"

    | Membership -> "mem" | Subset -> "subset" | Disjoint -> "disjoint"
    | Union _ -> "union" | Inter _ -> "inter" | Diff -> "diff" | Compl -> "compl"
    | Enum _ -> "set" | Universe _ -> "universe"
    | ConstArray _ -> "const-arr" | Select -> "select" | Store -> "store"

    | Pure -> "pure"
    | Emp -> "emp"
    | HeapTerm field -> Format.asprintf "%s[.]" (MemoryModel0.Field.show field)
    | PointsTo def -> Format.asprintf "pto(%s)" (StructDef.show def)
    | Predicate (id, _) -> Identifier.show id
    | BlockBegin -> "begin"
    | BlockEnd -> "end"

    | GuardedNot -> "gneg"
    | Star -> "star"
    | Septraction -> "septraction"

  let get_sort app xs = match app with
    | Constant c -> Constant.get_sort c
    | And | Or | Not | Implies | Iff | Equal | Distinct | Lesser | LesserEqual -> Sort.bool
    | Pure | Emp | PointsTo _ | Predicate _ | Star | Septraction | GuardedNot -> Sort.bool
    | Membership | Subset | Disjoint -> Sort.bool
    | BitCheck | BitUnsignedLesser | BitUnsignedLesserEqual -> Sort.bool
    | Plus | Minus | Mult -> Sort.int
    | Union sort | Inter sort | Enum sort | Universe sort -> sort
    | Diff | Compl -> List.hd xs
    | BitPlus width | BitAnd width | BitOr width | BitXor width -> Sort.mk_bitvector width
    | BitNot | BitShiftLeft | BitShiftRight | BitImplies | BitCompl -> List.hd xs
    | HeapTerm (field) -> Field.get_sort field
    | IfThenElse -> List.nth xs 1
    | ConstArray sort -> List.nth xs 0
    | Select -> Sort.get_range_sort @@ List.nth xs 0
    | Store -> List.hd xs
    | BlockBegin | BlockEnd -> List.hd xs

end

type t =
  | Variable of Variable.t
  | Application of Application.t * t List.t
  | Binder of binder * Variable.t List.t * t

and range = (t List.t Lazy.t [@ignore])

and binder =
  | Exists of (range List.t option [@ignore])
  | Forall of (range List.t option [@ignore])
  | Exists2 of (range List.t option [@ignore])
  | Forall2 of (range List.t option [@ignore])
  (*| Let*)
[@@deriving equal, compare]

module Binder = struct

  type nonrec range = range

  type t = binder

  let equal = equal_binder

  let compare = compare_binder

  let show = function
    | Exists None -> "exists"
    | Exists _ -> "exists <range>"
    | Forall None -> "forall"
    | Forall _ -> "forall <range>"

    | Exists2 None -> "exists2"
    | Exists2 _ -> "exists2 <range>"
    | Forall2 None -> "forall2"
    | Forall2 _ -> "forall2 <range>"
    (*| Let -> "let"*)

  let is_quantifier _ = (*function Let -> false | _ ->*) true

  let get_sort binder body_sort = (*match binder with
    | Let -> body_sort
    | _ -> *) Sort.bool

end

let rec show = function
  | Variable var -> Variable.show var
  | Application (app, []) -> Application.show app
  | Application (app, xs) ->
    Format.asprintf "(%s %s)"
      (Application.show app)
      (String.concat " " @@ List.map show xs)
  | Binder (binder, vars, x) ->
    Format.asprintf "(%s (%s) %s)"
      (Binder.show binder)
      (String.concat " " @@ List.map Variable.show vars)
      (show x)

let rec get_sort = function
  | Variable var -> Variable.get_sort var
  | Application (app, xs) -> Application.get_sort app (List.map get_sort xs)
  | Binder (binder, _, x) -> Binder.get_sort binder (get_sort x)

let has_sort sort x = Sort.equal sort (get_sort x)

let show_with_sort x = Format.asprintf "%s : %s" (show x) (Sort.show @@ get_sort x)

let rec is_quantifier_free = function
  | Variable _ -> true
  | Application (_, xs) -> List.for_all is_quantifier_free xs
  | Binder (binder, _, x) ->
    if Binder.is_quantifier binder then false
    else is_quantifier_free x

module Self = struct
  type nonrec t = t
  let show = show
  let compare = compare
end

include Datatype.Printable(Self)
include Datatype.Comparable(Self)
include Datatype.Collections(Self)

let hash = Hashtbl.hash

let is_var = function Variable _ -> true | _ -> false
let is_atom = function Application (_, []) -> true | _ -> false (* TODO: var? *)
let is_constant = function Application (Constant c, []) -> true | _ -> false

let mk_var name sort = Variable (Variable.mk name sort)
let mk_fresh_var name sort = Variable (Variable.mk_fresh name sort)

let mk_app app xs = Application (app, xs)
let mk_constant c = mk_app (Constant c) []
let mk_binder binder vars x = match vars with
  | [] -> x
  | _ -> Binder (binder, vars, x)

let of_var var = Variable var
let of_const = mk_constant

let to_constant = function Application (Constant c, []) -> c | _ -> failwith "Not a constant"

(** Higher-order function *)

let rec map fn = function
  | Variable (v, sort) -> fn @@ Variable (v, sort)
  | Application (app, xs) -> fn @@ Application (app, List.map (map fn) xs)
  | Binder (binder, vs, x) -> fn @@ Binder (binder, vs, map fn x)

let map_vars fn = map (function Variable v -> fn v | other -> other)

let rec map_app fn = function
  | Variable (v, sort) -> Variable (v, sort)
  | Application (app, xs) ->
    begin
      try fn app (List.map (map_app fn) xs)
      with Match_failure _ -> Application (app, List.map (map_app fn) xs) (* TODO: exit only once!! *)
    end
  | Binder (binder, vs, x) -> Binder (binder, vs, map_app fn x)


(** Predicates *)

let rec for_all pred phi = match phi with
  | Variable _ -> pred phi
  | Application (_, xs) -> pred phi && List.for_all (for_all pred) xs
  | Binder (_, _, x) -> pred phi && for_all pred x

let rec exists pred phi = match phi with
  | Variable _ -> pred phi
  | Application (_, xs) -> pred phi && List.exists (exists pred) xs
  | Binder (_, _, x) -> pred phi || for_all pred x

let for_all_apps pred =
  for_all (function Application (app, _) -> pred app | _ -> true)

(** Variables & terms *)

let rec get_vars phi =
  let vars = match phi with
    | Variable var -> [var]
    | Application (_, xs) -> List.concat_map get_vars xs
    | Binder (_, vs, x) -> vs @ get_vars x
  in
  BatList.unique ~eq:Variable.equal vars

let free_vars phi =
  let rec collect_vars bounded = function
    | Variable var ->
      if BatList.mem_cmp Variable.compare var bounded then []
      else [var]
    | Application (_, xs) -> List.concat_map (collect_vars bounded) xs
    | Binder (_, vs, x) -> collect_vars (vs @ bounded) x
  in
  collect_vars [] phi
  |> BatList.unique ~eq:Variable.equal

let free_vars_of_sort sort phi =
  List.filter (Variable.has_sort sort) (free_vars phi)

let get_all_sorts phi =
  free_vars phi
  |> List.map Variable.get_sort
  |> BatList.unique ~eq:Sort.equal

let rename_var old_name new_name =
  map_vars (fun var ->
    let name, sort = Variable.describe var in
    if String.equal name old_name
    then mk_var new_name sort
    else of_var var
  )

(** Subformulae *)

let rec select_subformulae pred phi =
  let acc = match phi with
    | Variable _ -> []
    | Application (_, xs) -> BatList.concat_map (select_subformulae pred) xs
    | Binder (_, _, x) -> select_subformulae pred x
  in
  if pred phi then phi :: acc else acc

let positive_polarity phi psi =
  let open ThreeValuedLogic in
  let rec aux chi =
    if equal psi chi then True
    else match chi with
      | Variable _ -> Unknown
      | Application ((GuardedNot | Not), xs) -> not3 @@ exists aux xs
      | Application (_, xs) -> exists aux xs
      | Binder ((Forall _ | Forall2 _), _, x) -> not3 @@ aux x
      | Binder ((Exists _ | Exists2 _), _, x) -> aux x
   in
   to_bool false @@ aux psi


(** Substitutions *)

let substitute term ~var ~by =
  let rec substitute_aux bounded term = match term with
    | Variable v ->
      if BatList.mem_cmp Variable.compare v bounded then Variable v
      else if Variable.equal v var then by
      else Variable v
    | Application (app, xs) -> Application (app, List.map (substitute_aux bounded) xs)
    | Binder (binder, vs, x) -> Binder (binder, vs, substitute_aux (vs @ bounded) x)
  in
  substitute_aux [] term

let rec replace_subformula term ~subformula ~by =
  if equal term subformula then by
  else
    let rec_call = fun t -> replace_subformula t ~subformula ~by in
    match term with
    | Variable _ -> term
    | Application (app, xs) -> Application (app, List.map rec_call xs)
    | Binder (binder, vs, x) -> Binder (binder, vs, rec_call x)


let substitute_list phi ~vars ~by =
  assert (List.length vars = List.length by);
  List.fold_left2 (fun phi var by -> substitute phi ~var ~by) phi vars by

let rec (===) lhs rhs = match lhs, rhs with
  | Variable v1, Variable v2 -> Variable.equal v1 v2

  | Application (a1, xs1), Application (a2, xs2) ->
    if not @@ Application.equal a1 a2 then false
    else if not @@ Application.can_reorder a1 then List.equal (===) xs1 xs2
    else List.equal (===) (List.sort compare xs1) (List.sort compare xs2)

  (* Alpha equivalence *)
  | Binder (b1, vs1, x1), Binder (b2, vs2, x2) ->
    if not @@ Binder.equal b1 b2 then false
    else if List.compare_lengths vs1 vs2 <> 0 then false
    else
      let fresh = List.map (fun x -> of_var @@ Variable.refresh x) vs1 in
      let x1' = substitute_list x1 ~vars:vs1 ~by:fresh in
      let x2' = substitute_list x2 ~vars:vs2 ~by:fresh in
      x1' === x2'

  | _ -> false

let get_sort_in_term name term =
  let vars = free_vars term in
  List.find (fun var -> String.equal (Variable.show var) name) vars
  |> Variable.get_sort

let get_sorts phi =
  select_subformulae (fun _ -> true) phi
  |> List.map get_sort
  |> BatList.unique ~eq:Sort.equal

let get_operands = function
  | Variable _ -> []
  | Application (_, xs) -> xs
  | Binder (_, _, x) -> [x]

(** Constructors *)

let cnt = ref 0

let mk_smart_app app ?neutral ?anihilator
  (*?(commutativy=false)
    ?(associative=false)*)
  operands =
    let is_neutral x = match neutral with Some n when equal x n -> true | _ -> false in
    let is_anihilator x = match anihilator with Some a when equal x a -> true | _ -> false in
    let operands =
      if List.exists is_anihilator operands then [Option.get anihilator]
      else List.filter (fun x -> not @@ is_neutral x) operands
    in
    let neutral = Option.value neutral ~default:(Application (app, [])) in
    let operands' = List.fold_left (fun acc -> function
      | Application (app', xs') when Application.equal app app' -> acc @ xs'
      | x -> acc @ [x]
    ) [] operands in
    match operands' with
    | [] -> neutral
    | [x] -> x
    | xs -> Application (app, xs)


module DefaultVars = struct
  let mk_var = mk_var
  let mk_fresh_var = mk_fresh_var
end

module Equality = struct
  let mk_eq = mk_app Equal
  let mk_distinct = mk_app Distinct

  let mk_eq2 x y = mk_eq [x; y]
  let mk_distinct2 x y = mk_distinct [x; y]
end

module Boolean = struct

  include Equality

  let mk_var name = mk_var name Sort.bool
  let mk_fresh_var name = mk_fresh_var name Sort.bool

  let mk_const c = mk_constant (Constant.mk_bool c)
  let tt = mk_const true
  let ff = mk_const false

  (** We can never simplify pure(phi) /\ emp ~> pure(phi) to be able
      to represent formulae in imprecise semantics. *)
  let mk_and = mk_smart_app And ~neutral:tt ~anihilator:ff

  let mk_or = mk_smart_app Or  ~neutral:ff ~anihilator:tt

  let mk_implies lhs rhs = mk_app Implies [lhs; rhs]
  let mk_iff = mk_app Iff

  let mk_not = function
    | Application (Equal, [x; y]) -> Equality.mk_distinct [x; y]
    | Application (Distinct, [x; y]) -> Equality.mk_eq [x; y]
    | other -> mk_app Not [other]

  let mk_ite cond b_then b_else = mk_app IfThenElse [cond; b_then; b_else]

  let rec mk_multiple_ite cases t_else = match cases with
    | (c, t) :: rest -> mk_ite c t (mk_multiple_ite rest t_else)
    | [] -> t_else

  (** Some syntax sugar *)
  let mk_and2 x y = mk_and [x; y]
  let mk_or2  x y = mk_or [x; y]

end

module Enumeration = struct

  include DefaultVars
  include Equality

  let mk_sort = Sort.mk_finite

  let mk_const sort name = mk_constant (Constant.mk_const sort name)

  let get_constants sort =
    Sort.get_constant_names sort
    |> List.map (Constant.mk_const sort)

  let get_constants_terms sort =
    Sort.get_constant_names sort
    |> List.map (mk_const sort)

end

module Arithmetic = struct

  include Equality

  let mk_var name = mk_var name Sort.int
  let mk_fresh_var name = mk_fresh_var name Sort.int

  let mk_const n = mk_constant (Constant.mk_int n)
  let zero = mk_const 0
  let one  = mk_const 1

  let mk_plus xs = mk_smart_app Plus ~neutral:zero xs
  let mk_minus lhs rhs = mk_app Minus [lhs; rhs]
  let mk_mult = mk_smart_app Mult ~neutral:one ~anihilator:zero

  let mk_lesser lhs rhs = mk_app Lesser [lhs; rhs]
  let mk_lesser_eq lhs rhs = mk_app LesserEqual [lhs; rhs]

end

module Sets = struct

  include Equality
  include DefaultVars

  (** Utility functions for constant sets *)

  let is_constant = function Application (Enum _, _) -> true | _ -> false

  let as_constant = function
    | Application (Enum _, xs) -> Set.of_list xs
    | set -> raise @@ Invalid_argument ("Not a constant set " ^ show set)

  let mk_sort = Sort.mk_set

  let mk_constant sort =
    assert (Sort.is_set sort);
    mk_app (Enum sort)

  let mk_empty sort =
    assert (Sort.is_set sort);
    mk_constant sort []

  let mk_universe sort =
    assert (Sort.is_set sort);
    mk_app (Universe sort) []

  let mk_singleton elem =
    mk_constant (Sort.mk_set @@ get_sort elem) [elem]

  let mk_union sort =
    assert (Sort.is_set sort);
    mk_smart_app (Union sort) ~neutral:(mk_empty sort) ~anihilator:(mk_universe sort)

  let mk_inter sort =
    assert (Sort.is_set sort);
    mk_smart_app (Inter sort) ~neutral:(mk_universe sort) ~anihilator:(mk_empty sort)

  let mk_disjoint = mk_app Disjoint

  let mk_add set elem = mk_union (Sort.mk_set @@ get_sort elem) [set; mk_singleton elem]
  let mk_diff lhs rhs = mk_app Diff [lhs; rhs]
  let mk_compl set = mk_app Compl [set]
  let mk_mem elem set = mk_app Membership [elem; set]
  let mk_subset lhs rhs = mk_app Subset [lhs; rhs]
  let mk_eq_empty set = mk_eq [set; mk_empty @@ get_sort set]
  let mk_eq_singleton set elem = mk_eq [set; mk_singleton elem]

  let may_disjoint xs =
    try
      let xs = List.map as_constant xs in
      List_utils.diagonal_product xs
      |> List.for_all (fun (x, y) -> Set.disjoint x y)
    (* One of sets is symbolic *)
    with Invalid_argument _ -> false


  let get_elem_sort set = Sort.get_dom_sort @@ get_sort set

end

module Array = struct

  include Equality
  include DefaultVars

  let mk_sort = Sort.mk_array

  let mk_const c dom_sort = mk_app (ConstArray dom_sort) [c]

  let mk_select arr index =
    assert (Sort.is_array @@ get_sort arr);
    assert (Sort.equal (get_sort index) (Sort.get_dom_sort @@ get_sort arr));

    mk_app Select [arr; index]

  let mk_store arr index value = mk_app Store [arr; index; value]

  let rec mk_nary_select n arr index = match n with
    | 0 -> index
    | n -> mk_nary_select (n - 1) arr (mk_select arr index)

end

module Bitvector = struct

  include Equality
  include DefaultVars

  let mk_sort width = Sort.mk_bitvector width

  let mk_const bv = mk_app (Constant (Constant.mk_bitvector bv)) []
  let mk_const_of_int i width = mk_const @@ Bitvector.of_int i width
  let mk_const_of_string str = mk_const @@ Bitvector.of_string str

  let get_width bv = Sort.get_width @@ get_sort bv

  let mk_zero width = mk_const @@ Bitvector.zero width
  let mk_one width = mk_const @@ Bitvector.one width
  let mk_full_zeros width = mk_const @@ Bitvector.full_zeros width
  let mk_full_ones width = mk_const @@ Bitvector.full_ones width

  (* TODO: simplify *)
  let mk_bit_check bv index = mk_app BitCheck [bv; index]

  let mk_not bv = mk_app BitNot [bv]

  let mk_plus width xs =
    let neutral = mk_full_ones width in
    mk_smart_app ~neutral (BitPlus width) xs

  let mk_and width =
    let neutral = mk_full_ones width in
    let anihilator = mk_full_zeros width in
    mk_smart_app ~neutral ~anihilator (BitAnd width)

  let mk_or width =
    let neutral = mk_full_zeros width in
    let anihilator = mk_full_ones width in
    mk_smart_app ~neutral ~anihilator (BitOr width)

  let mk_xor width = mk_app (BitXor width)
  let mk_implies lhs rhs = mk_app BitImplies [lhs; rhs]
  let mk_compl bv = mk_app BitCompl [bv]

  let mk_shift_left bv shift = mk_app BitShiftLeft [bv; shift]
  let mk_shift_right bv shift = mk_app BitShiftRight [bv; shift]

  let mk_lesser bv1 bv2 = mk_app BitUnsignedLesser [bv1; bv2]
  let mk_lesser_eq bv1 bv2 = mk_app BitUnsignedLesserEqual [bv1; bv2]

end

module Quantifiers = struct

  (* TODO: smart constructors for binders *)

  let mk_forall vars ?ranges t =
    mk_binder (Forall ranges) vars t

  let mk_exists vars ?ranges t =
    mk_binder (Exists ranges) vars t

  let mk_exists' sorts constructor =
    let binders = List.map (Variable.mk_fresh "e") sorts in
    let terms = List.map (of_var) binders in
    mk_exists binders (constructor terms)

  let mk_forall' sorts constructor =
    let binders = List.map (Variable.mk_fresh "e") sorts in
    let terms = List.map (of_var) binders in
    mk_forall binders (constructor terms)


  let mk_forall2 vars ?ranges t =
    mk_binder (Forall2 ranges) vars t

  let mk_exists2 vars ?ranges t =
    mk_binder (Exists2 ranges) vars t

  let mk_forall2_range vars ranges t = mk_binder (Forall2 ranges) vars t
  let mk_exists2_range vars ranges t = mk_binder (Exists2 ranges) vars t

end

module SeparationLogic = struct

  module ID = Identifier.Make ()

  let nil = of_var @@ Variable.nil

  let mk_block_begin x = mk_app BlockBegin [x]

  let mk_block_end x = mk_app BlockEnd [x]

  let mk_heap_term field source = mk_app (HeapTerm field) [source]

  let mk_pure phi =
    assert (Sort.is_bool @@ get_sort phi);
    mk_app Pure [phi]

  let emp = mk_app Emp []

  let mk_star psis = mk_smart_app Star ~neutral:emp psis

  let mk_septraction lhs rhs = mk_app Septraction [lhs; rhs]
  let mk_wand lhs rhs = Boolean.mk_not @@ mk_septraction lhs (Boolean.mk_not rhs)

  let mk_pto_struct x s ys = mk_app (PointsTo s) (x :: ys)

  let mk_pto x y = mk_pto_struct x StructDef.ls [y]

  (*
  let mk_pto_dls x n p = mk_pto_struct x StructDef.dls [n; p]
  let mk_pto_nls x n t = mk_pto_struct x StructDef.nls [n; t]
  *)

  let mk_predicate name ?(structs=[]) xs = mk_app (Predicate (ID.mk name, structs)) xs
  let mk_ls x y = mk_predicate "ls" [x; y]
  let mk_dls x y f l = mk_predicate "dls" [x; y; f; l]
  let mk_nls x y z = mk_predicate "nls" [x; y; z]

  let mk_gneg lhs rhs = mk_app GuardedNot [lhs; rhs]

  let is_nil x = equal x nil

end


(** TODO *)


  (** Higher-order functions *)


  (** {2 Scanning & Searching} *)

  let rec exists_app predicate = function
    | Variable _ -> false
    | Application (app, xs) -> predicate app || List.exists (exists_app predicate) xs
    | Binder (_, _, x) -> exists_app predicate x

  (*
  let map_app term fn = map term (function
    | Application (app, xs) -> Application (fn app, xs)
    | other -> other
  )
  *)

  (*
  let rec fold term fn acc = match term with
    | Variable v -> fn acc @@ Variable v
    | Application (app, xs) -> fn acc xs
    | Binder (binder, vs, x) -> fn acc @@ Binder (binder, vs, map fn x)
  *)


  (** Others *)

  let rec size = function
    | Variable _ -> 1
    | Application (_, psis) -> 1 + (BatList.sum @@ List.map size psis)
    | Binder (_, xs, psi) -> List.length xs + size psi


  let output_benchmark path phi status =
    let channel = open_out path in
    (* TODO *)
    close_out channel

(* ----------------------------------------------------------------------------
   Pretty dot ouput

   TODOs:
     - should we always do folding?

*)

module U = UnicodeSymbols

module Vertex = struct

  type t = Int.t Option.t * String.t [@@deriving compare, equal]

  let hash = Hashtbl.hash

  let show (_, name) = name

  let show_full (tag, name) = match tag with
    | None -> name
    | Some i -> Format.asprintf "\"%d: %s\"" i name
end

(** Integer as edge labels should ensure that children are sorted as intended. *)
module Edge = struct
  include Int
  let default = 0
end

module G = struct
  module Self = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge)
  include Self
  include Graph.Oper.P(Self)
end

module DotConfig = struct
  include G
  let graph_attributes _ = [`Rankdir `TopToBottom]
  let default_vertex_attributes _ = []
  let vertex_name v = Vertex.show_full v
  let vertex_attributes v = [`Label (Vertex.show v)]
  let get_subgraph _ = None

  let default_edge_attributes _ = []
  let edge_attributes _ = []
end

module Dot = Graph.Graphviz.Dot(DotConfig)

type ast = G.t

(** Pretty application *)

type action =
  | Stop of string (* Show string and do not continue with sub-trees *)
  | Continue of string (* Show string and continue with sub-trees *)
  | ContinueWith of string * t list (* Continue with modified sub-trees *)

let rec fold_heap_term field ?(n=1) = function
  | Application (HeapTerm field', [x]) when Field.equal field field' ->
    fold_heap_term field ~n:(n+1) x
  | x ->
    let folded = Format.asprintf "%s^%d" (Field.show field) n in
    if is_var x then Stop (Format.asprintf "%s[%s]" folded (show x))
    else ContinueWith (folded, [x])

let rec fold_select arr ?(n=1) = function
  | Application (Select, [arr'; x]) when equal arr arr' ->
    fold_select arr ~n:(n+1) x
  | x ->
    let n = if n = 1 then "" else "^" ^ string_of_int n in
    if is_var arr && is_var x then Stop (Format.asprintf "%s%s[%s]" (show arr) n (show x))
    else if is_var arr then ContinueWith (Format.asprintf "%s%s[.]" (show arr) n, [x])
    else if is_var x then ContinueWith (Format.asprintf "[.]%s[%s]" n (show x), [arr])
    else Continue (Format.asprintf "[.]%s[.]" n)

(** TODO:
    - make the construction bottom-up for even prettier printing
    - constant sets *)
let pretty_node_name =
  let show = show_with_sort in
  function
  (* Introduce syntax sugar *)
  | Application (HeapTerm f, [x]) -> fold_heap_term f x
  | Application (Select, [arr; index]) -> fold_select arr index

  | Application (Equal, [x; y]) when is_var x && is_var y ->
    Stop (Format.asprintf "%s %s %s" (show x) !U.eq (show y))

  | Application (Distinct, [x; y]) when is_var x && is_var y ->
    Stop (Format.asprintf "%s %s %s" (show x) !U.neq (show y))

  | Application (Predicate (name, _), xs) when List.for_all is_var xs ->
    Stop (Format.asprintf "%s(%s)" (Identifier.show name) (show_list xs))

  | Application (Not, [Application (Emp, [])]) ->
    Stop (Format.asprintf "%s emp" !U.not)

  | Application (Enum _, []) -> Stop !U.empty_set
  | Application (Equal, _) -> Continue !U.eq
  | Application (Distinct, _) -> Continue !U.neq
  | Application (And, _) -> Continue !U.and_
  | Application (Or, _) -> Continue !U.or_
  | Application (Not, _) -> Continue !U.not
  | Application (GuardedNot, _) -> Continue (!U.and_ ^ !U.not)
  | Application (Star, _) -> Continue !U.star
  | Application (Septraction, _) -> Continue !U.septraction

  | Binder (Exists _, xs, _) -> Continue (!U.exists ^ Variable.show_list xs)
  | Binder (Forall _, xs, _) -> Continue (!U.forall ^ Variable.show_list xs)
  | Binder (Exists2 _, xs, _) -> Continue (!U.exists ^ "2" ^ Variable.show_list xs)
  | Binder (Forall2 _, xs, _) -> Continue (!U.forall ^ "2" ^ Variable.show_list xs)

  | Application (app, _) -> Continue (Application.show app)
  | Variable _ -> assert false

let to_ast ?(dagify=false) term =
  (* Create single node *)
  let mk_node kind n = if dagify then (None, kind) else (Some n, kind) in

  (* Recursively builds sub-trees *)
  let rec builder tag = function
    | Variable v ->
      let node = mk_node (Variable.show_with_sort v) tag in
      let g = G.add_vertex G.empty node in
      (g, node, tag + 1)
    | node ->
      let name, sub_trees = match pretty_node_name node with
        | Stop str -> str, []
        | Continue str -> str, get_operands node
        | ContinueWith (str, sub) -> str, sub
      in
      (* Construct sub-trees *)
      let root = mk_node name tag in
      let g, next_tag = BatList.fold_left (fun (acc, tag) x ->
        let g, root', next_tag = builder tag x in
        let acc = G.union g acc in
        (G.add_edge_e acc (root, tag, root'), next_tag)
      ) (G.empty, tag + 1) sub_trees
      in
      (g, root, next_tag)
  in
  match term with
    | Application (GuardedNot, [lhs; rhs]) ->
      let root = mk_node !U.entails 0 in
      let lhs, lhs_root, next_tag  = builder 1 lhs in
      let rhs, rhs_root, _ = builder next_tag rhs in
      let g = G.union lhs rhs in
      let g = G.add_edge_e g (root, 0, lhs_root) in
      G.add_edge_e g (root, 1, rhs_root)

    | _ -> match builder 0 term with (ast, _, _) -> ast

let output_ast path g =
  let channel = open_out path in
  Dot.output_graph channel g;
  close_out channel
