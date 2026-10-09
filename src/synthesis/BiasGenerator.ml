open MemoryModel

let concat sep = function
  | [x] -> Format.sprintf "%s," x
  | xs -> String.concat "," xs

let (+++) str1 str2 = Format.asprintf "%s\n%s" str1 str2

let template = [%blob "bias_template.pl"]

let predicate_def name n =
  let typ = concat ", " @@ List.init n (fun _ -> "node") in
  let direction = concat ", " @@ List.init (n) (fun _ -> "in") in
  Format.asprintf "head_pred(%s, %d)." name (n)
  +++ Format.asprintf "type(%s, (%s))." name typ
  +++ Format.asprintf "direction(%s, (%s))." name direction

let body_predicate_defs predicates =
  String.concat "\n\n" @@ List.map (fun (p, n) ->
    let typ = concat ", " @@ List.init n (fun _ -> "node") in
    let direction = concat ", " @@ List.init (n) (fun _ -> "in") in
        Format.asprintf "body_pred(%s, %d)." p n
    +++ Format.asprintf "type(%s, (%s))." p typ
    +++ Format.asprintf "direction(%s, (%s))." p direction
  ) predicates

let fields_defs fields =
  String.concat "\n" @@ List.map (fun f ->
    let f = Field.show f in
        Format.asprintf "body_pred(%s,2)." f
    +++ Format.asprintf "type(%s, (node, node))." f
    +++ Format.asprintf "direction(%s, (in, out))." f
    +++ Format.asprintf "body_pred(%s_is_nil, 1)." f
    +++ Format.asprintf "type(%s_is_nil, (node, ))." f
    +++ Format.asprintf "direction(%s_is_nil, (in, ))." f
    +++ Format.asprintf "%s_class(%s)." f f
    +++ Format.asprintf "%s_class(%s_is_nil)." f f
  ) fields

let field_constraints name n fields =
  let gen field =
    let f = Field.show field in
    let root_vars = String.concat ", " @@ ("T" :: List.init (n-1) (fun _ -> "_")) in
    Format.asprintf ":- inductive_case(R), #count{P, Vars : body_literal(R, P, _, Vars), %s_class(P)} != 1." f
    (* Fields are only defined for root *)
    +++ Format.asprintf ":- body_literal(R, %s, 2, (T, _)), not root (R, T)." f
    +++ Format.asprintf ":- body_literal(R, %s_is_nil, 1, (T,)), not root (R, T)." f
  in
  String.concat "\n" @@ List.map gen fields

let helper_defs predicates fields n =
  let fields = List.map Field.show fields in
  let aux expected current =
    if Int.equal expected current then "V"
    else "_"
  in
  let gen n i =
    let str = List.init n (aux i) in
    Format.sprintf "(%s)" (String.concat ", " str)
  in
  let root_def = Format.sprintf "root(C, V) :- head_literal(C, _, %d, %s).\n" n (gen n 0) in
  let head_def =
    List.init n (fun i -> Format.sprintf "head_var(C, V) :- head_literal(C, _, %d, %s)." n (gen n i))
    |> String.concat "\n"
  in
  let target_def = String.concat "\n" @@ List.map (fun f -> Format.sprintf "target(R,T ) :- body_literal(R, %s, 2, (_, T))." f) fields in
  let established_def = String.concat "\n" @@ List.map (fun (p, n) ->
          Format.sprintf "established(R, V) :- body_literal(R, %s, %d, %s)." p n (gen n 0)
    ) predicates
  in
  root_def +++ head_def +++ target_def +++ established_def

let predicate_constraints name n =
  let header = String.concat "," @@ List.init n (fun n -> Format.sprintf "V%d" (n+1)) in
  let aux = String.concat ", " @@ List.init n (fun n -> Format.sprintf "not head_var(R, V%d)" (n+1)) in
  Format.sprintf ":- body_literal(R, %s, %d, (%s)), %s." name n header aux

let setting () =
  Format.sprintf "max_body(%d).\nmax_vars(%d)."
    6 (* body *)
    7 (* vars *)

let replace ~sub ~by str = snd @@ BatString.replace ~sub ~by ~str

let generate predicates name n fields =
  template
  |> replace ~sub:"__SETTING__" ~by:(setting ())
  |> replace ~sub:"__HEAD_PRED__" ~by:name
  |> replace ~sub:"__HEAD_PRED_DEF__" ~by:(predicate_def name n)
  |> replace ~sub:"__BODY_PRED_DEFS__" ~by:(body_predicate_defs (List.tl predicates))
  |> replace ~sub:"__HELPER_DEFS__" ~by:(helper_defs predicates fields n)
  |> replace ~sub:"__FIELD_DEFS__" ~by:(fields_defs fields)
  |> replace ~sub:"__FIELD_CONSTRAINTS__" ~by:(field_constraints name n fields)
  |> replace ~sub:"__PREDICATE_CONSTRAINTS__" ~by:(predicate_constraints name n)
