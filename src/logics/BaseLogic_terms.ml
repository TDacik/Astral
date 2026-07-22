(* This module serves as a base implementation for both first-order and separation logic.
 *
 * TODO: successor sorting in .dot output
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module Application = BaseLogic_application

module Variable = Variable.Make()
module Sort = Sort

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
  | Application (app, xs) when Application.show_kind app = Prefix ->
    Format.asprintf "(%s %s)"
      (Application.show app)
      (String.concat " " @@ List.map show xs)
  | Application (app, [x]) when Application.show_kind app = Infix ->
    (* TODO *)
      Format.asprintf "%s [%s]"
      (Application.show app)
      (show x)
  | Application _ -> assert false


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

let map_terms = map

let map' fn = function
  | Variable (v, sort) -> fn @@ Variable (v, sort)
  | Application (app, xs) -> fn @@ Application (app, xs)
  | Binder (binder, vs, x) -> fn @@ Binder (binder, vs, x)

let map_vars fn = map (function Variable v -> fn v | other -> other)

let rec map_app fn = function
  | Variable (v, sort) -> Variable (v, sort)
  | Application (app, xs) -> fn app (List.map (map_app fn) xs)
  | Binder (binder, vs, x) -> Binder (binder, vs, map_app fn x)

(** TODO: works only for SL (not implication etc.) *)
let skolemisation term =
  let rec perform is_positive term = match term with
    | Variable _ -> term, []
    | Application (Not, [x]) ->
      let x', xs = perform (not is_positive) x
      in Application (Not, [x']), xs
    | Application (GuardedNot, [lhs; rhs]) ->
      let lhs', xs1 = perform is_positive lhs in
      let rhs', xs2 = perform (not is_positive) rhs in
      Application (GuardedNot, [lhs'; rhs']), xs1 @ xs2
    | Application (app, ts) ->
      let ts', xs =
        List.fold_left (fun (t_acc, xs_acc) t ->
          let t', xs = perform is_positive t in (t_acc @ [t'], xs @ xs_acc)
        ) ([], []) ts
      in
      Application (app, ts'), xs
    | Binder (Exists _, xs, t) when is_positive ->
      let t', skolems = perform is_positive t in
      t', xs @ skolems
    | Binder (Forall r, xs, t) ->
      let t', skolems = perform (not is_positive) t in
      Binder (Forall r, xs, t'), skolems
    | Binder (binder, xs, t) ->
      let t', skolems = perform is_positive t in
      Binder (binder, xs, t'), skolems
  in
  perform true term

(** Predicates *)

let rec for_all pred phi = match phi with
  | Variable _ -> pred phi
  | Application (_, xs) -> pred phi && List.for_all (for_all pred) xs
  | Binder (_, _, x) -> pred phi && for_all pred x

let rec exists pred phi = match phi with
  | Variable _ -> pred phi
  | Application (_, xs) -> pred phi || List.exists (exists pred) xs
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

let rec bound_vars = function
  | Variable var -> []
  | Application (_, xs) -> List.concat_map bound_vars xs
  | Binder (_, vs, x) -> vs @ bound_vars x

let free_vars_of_sort sort phi =
  List.filter (Variable.has_sort sort) (free_vars phi)

let is_ground ~ground phi =
  let module S = Variable.Set in
  let vars = get_vars phi in
  S.subset (S.of_list vars) (S.of_list ground)

let is_ground' ~forbidden phi =
  let module S = Variable.Set in
  let vars = get_vars phi in
  S.disjoint (S.of_list vars) (S.of_list forbidden)

let get_all_sorts phi =
  get_vars phi
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
  let module TL = ThreeValuedLogic in
  let rec aux chi =
    if equal psi chi then TL.True
    else match chi with
      | Variable _ -> TL.Unknown
      | Application ((GuardedNot | Not), xs) -> TL.not3 @@ TL.exists aux xs
      | Application (_, xs) -> TL.exists aux xs
      | Binder ((Forall _ | Forall2 _), _, x) -> TL.not3 @@ aux x
      | Binder ((Exists _ | Exists2 _), _, x) -> aux x
   in
   TL.to_bool false @@ aux psi


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

      let rename = List.mapi (fun i x -> mk_var ("__e" ^ string_of_int i) (Variable.get_sort x)) in
      let x1' = substitute_list x1 ~vars:vs1 ~by:(rename vs1) in
      let x2' = substitute_list x2 ~vars:vs2 ~by:(rename vs2) in
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

(** Constructors

*)









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

