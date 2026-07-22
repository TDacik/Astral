(* Base logic as s-expressions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open MemoryModel

open BaseLogic_terms

module Boolean = BaseLogic_boolean.Smart

open Sexplib
module F = Format

let declare_var var =
  Format.asprintf "(declare-const %s)" (Variable.smt2_decl var)

let header with_decls phi source status options =
  let open PrintUtils in
  let source = match source with
    | None -> ""
    | Some source -> F.asprintf "(set-info :source %s)" source
  in
  let status = match status with
    | None | Some `Unknown -> "(set-info :status unknown)"
    | Some `Sat -> "(set-info :source sat)"
    | Some `Unsat -> "(set-info :source unsat)"
  in
  let options = match options with
    | None -> ""
    | Some options -> options
  in
  let vars = String.concat "\n" @@ List.map declare_var (BatList.remove (free_vars phi) Variable.nil) in
  source ++ status +++ options +++ vars

let binder_var var =
  Sexp.List [Sexp.Atom (Variable.show var); Sexp.Atom (Sort.name @@ Variable.get_sort var)]

(* TODO: names *)
type sexp_action =
  | App of string
  | Direct of (Sexp.t list -> Sexp.t)
  | Modify of string * t list
  | Skip

let app_to_sexp app xs = match app with
  | Application.Equal -> App "="
  | Application.Star -> App "sep"
  | Application.GuardedNot -> Modify ("and", [List.hd xs; Boolean.mk_not @@ List.nth xs 1])
  | Application.Pure -> Skip
  | Application.Constructor c -> App (StructDef.show_cons c)
  | Application.Cast sort ->
    Direct (fun xs -> Sexp.List [Sexp.Atom "as"; List.hd xs; Sexp.Atom (Sort.name sort)])

  | app -> App (Application.show app)

let rec to_sexp = function
  | Variable v -> Sexp.Atom (Variable.show v)
  | Application (app, []) -> Sexp.Atom (Application.show app)
  | Application (app, xs) ->
    begin match app_to_sexp app xs, xs with
      | App app, _ -> List (Sexp.Atom app :: List.map to_sexp xs)
      | Skip, [x] -> to_sexp x
      | Modify (app, xs'), _ -> List (Sexp.Atom app :: List.map to_sexp xs')
      | Direct fn, xs -> fn @@ List.map to_sexp xs
      | _ -> assert false
    end
  | Binder (binder, xs, phi) ->
    let binder = Sexp.Atom (Binder.show binder) in
    let vars = Sexp.List (List.map binder_var xs) in
    Sexp.List [binder; vars; to_sexp phi]

let to_smt2 phi = Sexp.to_string_hum @@ to_sexp phi

(* TODO: points-to LHS *)
let rec introduce_casts ?(expected=Sort.bool) (pred_sigs: (string * Sort.t list) list) (phi : t) =
  let recurse expected psi = introduce_casts ~expected pred_sigs psi in
  map' (fun node -> match node with
    | Variable v when Variable.is_nil v -> Application (Cast expected, [node])
    | Variable _ -> node
    | Application ((Equal | Distinct) as ap, xs) ->
      let expected =
        match List.filter (fun sort -> not @@ Sort.is_nil sort) @@ List.map get_sort xs with
         | [] -> Sort.loc_ls (* All polymorphic, sort is not relevant *)
         | sort :: _ -> sort
      in
      Application (ap, List.map (recurse expected) xs)
    | Application (Constructor def, xs) ->
      Application (Constructor def,
        List.mapi (fun i x ->
          let expected = Field.get_sort @@ List.nth def.fields i in
          recurse expected x
        ) xs
      )
    | Application (Predicate (name, instance), xs) when List.mem_assoc (Identifier.show name) pred_sigs ->
      let name_str = Identifier.show name in
      Application (Predicate (name, instance),
        List.mapi (fun i x ->
          let expected  = List.nth (List.assoc name_str pred_sigs) i in
          recurse expected x
        ) xs
      )
    | Application (Predicate (p, i), xs) -> Application (Predicate (p, i), xs)
    | Application (ap, xs) -> Application (ap, List.map (recurse expected) xs)
    | Binder (binder, xs, psi) -> Binder (binder, xs, recurse expected psi)
  ) phi

let to_sexp_aux phi =
  let f sexps = Sexp.to_string_hum @@ Sexp.List sexps in
  match phi with
  | Application (GuardedNot, [lhs; rhs]) ->
    let pre = f [Sexp.Atom "assert"; to_sexp lhs] in
    let post = f [Sexp.Atom "assert"; to_sexp @@ Boolean.mk_not rhs] in
    pre ^ "\n\n" ^ post
  | phi -> f [Sexp.Atom "assert"; to_sexp phi]

let to_bench ?(pred_sigs=[]) ?source ?status ?options phi =
  let header = header true phi source status options in
  let body = to_sexp_aux @@ introduce_casts pred_sigs phi in
  PrintUtils.(+++) header body

let output_benchmark ?(pred_sigs=[]) ?source ?status ?options path phi =
  let channel = open_out path in
  output_string channel @@ to_bench ~pred_sigs ?source ?status ?options phi;
  Out_channel.close channel
