(*
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Location_sig

module Make (Locations : LOCATIONS_BASE) = struct

  include Locations

  (** Auxiliary definitions *)

  let set_sort locs = SMT.Set.mk_sort (Locations.get_sort locs)


  (** Declaration *)

  let mk_var locs name = SMT.Variable.mk name (Locations.get_sort locs)

  let mk_fresh_var locs name = SMT.Variable.mk_fresh name (Locations.get_sort locs)

  let mk_set_var locs name = SMT.Variable.mk name (set_sort locs)

  let mk_fresh_set_var locs name = SMT.Variable.mk_fresh name (set_sort locs)


  (** Translation *)

  let translate_sort locs _ = Locations.get_sort locs

  let translate_var locs = function SSL.Var (name, _) -> mk_var locs name


  (** Typing *)

  let mk_of_type locs var sl_sort =
    SMT.Set.mk_mem var (Locations.get_sort_encoding locs sl_sort)

  let mk_set_of_type locs set sl_sort =
    SMT.Set.mk_subset set (Locations.get_sort_encoding locs sl_sort)

  (** Quantifiers *)

  let quantify locs quantifier expr_constructor =
    let binder = mk_fresh_var locs "loc" in
    let expr = expr_constructor binder in
    match quantifier with
    | `Forall -> SMT.Quantifier.mk_forall [binder] expr
    | `Exists -> SMT.Quantifier.mk_exists [binder] expr

  let mk_forall locs constructor = quantify locs `Forall constructor
  let mk_exists locs constructor = quantify locs `Exists constructor


  (** Axioms *)

  let var_axiom locs var =
    if SSL.Variable.is_nil var then
      let nil_const = Locations.nil_const locs in
      SMT.mk_eq (translate_var locs (Var var)) nil_const
    else
      let sort = SSL.Variable.get_sort var in
      let sort_set = Locations.get_sort_encoding locs sort in
      mk_of_type locs (translate_var locs (Var var)) sort

  let sort_axiom locs sort =
    let set = Locations.get_sort_encoding locs sort in
    let consts = Locations.get_constants_s locs sort in
    let set_def = SMT.Set.mk_enumeration (set_sort locs) consts in
    SMT.Set.mk_eq set set_def

  let axioms locs phi =
    let vars = SSL.Variable.nil :: SSL.get_vars phi in
    let sorts = SSL.get_loc_sorts ~with_nil:false phi in
    let var_axioms = SMT.Boolean.mk_and @@ List.map (var_axiom locs) vars in
    let sort_axioms = SMT.Boolean.mk_and @@ List.map (sort_axiom locs) sorts in
    SMT.Boolean.mk_and [var_axioms; sort_axioms]

  (** Utilities *)

  let rec powerset = function
    | [] -> [[]]
    | x :: xs ->
      let ps = powerset xs in
      ps @ List.map (fun s -> x :: s) ps

  let powerset locs = powerset (Locations.get_constants locs)

end

