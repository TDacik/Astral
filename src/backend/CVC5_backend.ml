(* cvc5 adapter for Astral
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Self = struct

  let name = "cvc5"
  let binary = "cvc5"

  let model_option = "--produce-models"
  let parser_implemented = true

  let supports_smtlib_options = true
  let supports_get_info = true
  let supports_sets = true
  let supports_quantifiers = true

  let default_options = ["--sygus-inst"; "--cegqi-full"]

  let translate_non_std translate translate_sort phi =
    let translate_list es = String.concat " " @@ List.map translate es in
    match phi with
    | SMT.Membership (e1, e2) ->
      Format.asprintf "(set.member %s %s)" (translate e1) (translate e2)
    | SMT.Subset (e1, e2) -> Format.asprintf "(set.subset %s %s)" (translate e1) (translate e2)
    | SMT.Union (es, _) -> Format.asprintf "(set.union %s)" (translate_list es)
    | SMT.Inter (es, _) -> Format.asprintf "(set.inter %s)" (translate_list es)
    | SMT.Diff (e1, e2) -> Format.asprintf "(set.minus %s %s)" (translate e1) (translate e2)
    | SMT.Compl e -> Format.asprintf "(set.complement %s)" (translate e)
    | SMT.Disjoint [e1; e2] ->
      Format.asprintf "(= (set.inter %s %s) (as set.empty %s))"
        (translate e1)
        (translate e2)
        (translate_sort @@ SMT.Set.get_sort e1)

    | SMT.Enumeration ([], sort) -> Format.asprintf "(as set.empty %s)" (translate_sort sort)
    | SMT.Enumeration (es, sort) ->
      Format.asprintf "(set.insert %s (as set.empty %s))"
        (translate_list es)
        (translate_sort sort)

  let translate_non_std_sort translate_sort = function
    | Sort.Set (elem_sort) -> "(Set " ^ translate_sort elem_sort ^ ")"
    | Sort.Finite (name, consts) -> name

  let declare_non_std_sort = function
    | Sort.Set (elem_sort) -> ""
    | Sort.Finite (name, consts) ->
      let constructors = String.concat " " @@ List.map (fun c -> "(|" ^ c ^ "|)") consts in
      Format.asprintf "(declare-datatypes ((%s 0)) ((%s)))" name constructors

end

include Smtlib_backend_builder.Make(Self)
