open Logic_sig

module Make (Term : TERM) = struct

  include Term

  let node_type term = snd @@ Term.describe_node term

  let rec node_name term = match node_type term with
    | Var _ | Operator _ | Connective _ -> fst @@ Term.describe_node term
    | Quantifier (binders, body) ->
      Format.asprintf "%s %s"
        (fst @@ Term.describe_node term)
        (String.concat ", " @@ List.map node_name binders)

  (* TODO: avoid frequent string comparison *)
  let rec compare term1 term2 = match node_type term1, node_type term2 with
    | Var (name1, sort1), Var (name2, sort2) ->
        let cmp = String.compare name1 name2 in
        if cmp != 0 then cmp
        else Sort.compare sort1 sort2
    | Operator (subterms1, _), Connective subterms2
    | Connective subterms1, Operator (subterms2, _)
    | Operator (subterms1, _), Operator (subterms2, _)
    | Connective subterms1, Connective subterms2 ->
        let name1 = node_name term1 in
        let name2 = node_name term2 in
        let cmp = String.compare name1 name2 in
        if cmp != 0 then cmp
        else List.compare compare subterms1 subterms2
    | Quantifier (binders1, body1), Quantifier (binders2, body2) ->
        let cmp = List.compare compare binders1 binders2 in
        if cmp != 0 then cmp
        else compare body1 body2
    | _ -> Stdlib.compare term1 term2

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

  let rec get_vars term = match node_type term with
    | Var _ -> [term]
    | Operator (terms, _) | Connective terms -> List.concat @@ List.map free_vars terms
    | Quantifier (_, phi) -> get_vars phi

  let get_operands phi = match node_type phi with
    | Var _ -> []
    | Operator (terms, _) | Connective terms -> terms
    | Quantifier (_, term) -> [term]

  let get_all_sorts term =
    free_vars term
    |> List.map get_sort
    |> List.sort_uniq Sort.compare

  (* TODO: fix single variable *)
  let rec size term = match node_type term with
    | Var _ -> 0
    | Operator (terms, _) | Connective terms ->
        List.fold_left (fun acc x -> acc + size x) 1 terms
    | Quantifier (binders, phi) ->
        List.length binders + size phi

  (* ==== Higher-order functions ==== *)

  let rec for_all predicate phi =
    let acc = predicate phi in
    match node_type phi with
    | Var _ -> acc
    | Operator (terms, _) | Connective terms -> acc && List.for_all (for_all predicate) terms
    | Quantifier (_, psi) -> acc && for_all predicate psi

  let rec exists predicate phi =
    let acc = predicate phi in
    match node_type phi with
    | Var _ -> acc
    | Operator (terms, _) | Connective terms -> acc || List.exists (exists predicate) terms
    | Quantifier (_, psi) -> acc || exists predicate psi

  (* ==== Properties ==== *)

  let rec is_quantifier_free term = match node_type term with
    | Var _ -> true
    | Operator (terms, _) | Connective terms -> List.for_all is_quantifier_free terms
    | Quantifier _ -> false

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
  | Quantifier _ -> "TODO: quantifier"

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

  let to_smtlib_bench term = failwith "Not implemented"

  let pretty term = match Term.pretty_print show term with
    | None -> node_name term
    | Some (`Node name) | Some (`Tree name) -> name

  (** Operations over AST as graph *)

  module AST = AST.Make
    (struct
      module Term = Term
      let node_name = pretty
      let get_operands t = match Term.pretty_print show t with
        | Some (`Tree _) -> []
        | _ -> get_operands t
    end)

  let dump_ast path (term : Term.t) : unit = AST.dump path (AST.make term)

  module Self = struct
    type t = Term.t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Comparable(Self)

  (** Collections are not directly included as the Logic instance can have
      submodules like `Set`. *)
  module Collections = Datatype.Collections(Self)

end
