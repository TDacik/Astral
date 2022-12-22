open Logic_sig

module Make (Term : TERM) = struct

  include Term

  let node_name term = fst @@ Term.describe_node term

  let node_type term = snd @@ Term.describe_node term

  let get_sort t = match node_type t with
    | Var (_, sort) -> sort
    | Operator (_, sort) -> sort
    | Connective _ -> Sort.Bool
    | Quantifier _ -> Sort.Bool

  (* ==== Syntactic manipulation ==== *)

  let rec is_constant term = match node_type term with
    | Var _ -> false
    | Operator (terms, _) | Connective terms -> List.for_all is_constant terms
    | Quantifier _ -> false

  let rec free_vars term = match node_type term with
    | Var _ -> [term]
    | Operator (terms, _) | Connective terms -> List.concat @@ List.map free_vars terms
    | Quantifier (xs, phi) ->
        List.filter (fun x -> not @@ BatList.mem_cmp compare x xs) (free_vars phi)

  let free_vars term = List.sort_uniq compare (free_vars term)

  let rec size term = match node_type term with
    | Var _ -> 1
    | Operator (terms, _) | Connective terms ->
        List.fold_left (fun acc x -> acc + size x) 1 terms
    | Quantifier (binders, phi) ->
        List.length binders + size phi


  (* ==== Printing ==== *)

  (** Default show using s-expressions *)
  let (++) = (^)

  let mk_indent n = String.init n (fun _ -> ' ')

  let rec pretty_terms_line terms =
    List.map (pretty 0) terms
    |> String.concat " "

  and pretty_terms n terms =
    List.map (pretty n) terms
    |> String.concat ("\n" ++ mk_indent n)

  and pretty n node = match node_type node with
  | Var (name, sort) ->
      mk_indent n ++ name
  | Operator (terms, sort) ->
      if size node < 5 then
        mk_indent n ++ "(" ++ node_name node ++ " " ++ pretty_terms_line terms ++ ")"
      else
        mk_indent n ++ "(" ++ node_name node ++ "\n" ++ pretty_terms (n + 2) terms ++ mk_indent n ++ ")\n\n"
  | Connective terms ->
      if size node < 6 then
        mk_indent n ++ "(" ++ node_name node ++ " " ++ pretty_terms_line terms ++ ")"
      else
        mk_indent n ++ "(" ++ node_name node ++ "\n" ++ pretty_terms (n + 2) terms ++ mk_indent n ++ ")\n\n"
  | Quantifier _ -> "TODO"

  let rec show term = (*pretty 0 term*)
    match node_type term with
    | Var (name, _) -> name
    | Operator (terms, _) | Connective terms ->
      begin match terms with
        | [] -> Format.asprintf "(%s)" (node_name term)
        | terms ->
          let operands = String.concat " " (List.map show terms) in
          Format.asprintf "(%s %s)" (node_name term) operands
      end
    | Quantifier (binders, phi) ->
      let binders = String.concat " " (List.map show_with_sort binders) in
      Format.asprintf "(%s %s. %s)" (node_name term) binders (show phi)

  and show_with_sort term = match node_type term with
    | Quantifier _ -> show term
    | _ -> Format.asprintf "%s : %s" (show term) (Sort.show @@ get_sort term)

  module Self = struct
    type t = Term.t
    let show = show
  end

  include Datatype.Printable(Self)

end
