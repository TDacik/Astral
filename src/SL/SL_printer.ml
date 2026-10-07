(* Customizable printing of SL formulae.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

open SL
open MemoryModel

let (++) = (^)

let pretty_eq xs =
  String.concat " = " @@ List.map SL.Term.show xs

let pretty_distinct = function
  | [x; y] -> Format.asprintf "%s %s %s" (SL.Term.show x) !UnicodeSymbols.neq (SL.Term.show y)
  | xs -> Format.asprintf "distinct(%s)" @@ SL.Term.show_list xs

let pretty_pointsto x ys =
  Format.asprintf "%s %s <%s>"
    (SL.Term.show x)
    !UnicodeSymbols.maps_to
    (SL.Term.show_list ys)

let pretty_atom psi = match SL.view psi with
  | Emp -> "emp"
  | Eq xs -> pretty_eq xs
  | Distinct xs -> pretty_distinct xs
  | PointsTo (x, _, ys) -> pretty_pointsto x ys
  | Predicate (name, xs, 0, _) ->
    Format.asprintf "%s(%s)" name (SL.Term.show_list xs)

let pretty_binder = function
  | [] -> ""
  | xs ->
    Format.asprintf "%s %s. "
      !UnicodeSymbols.exists
      (SL.Variable.show_list xs)

let pretty_symbolic_heap phi =
  let qs, atoms = SL.as_quantified_symbolic_heap phi in
  if List.is_empty atoms then "emp"
  else
    pretty_binder qs
    ++ (String.concat (" " ^ !UnicodeSymbols.star ^ " ") @@ List.map pretty_atom atoms)


type printer = {
  eq : string;
  neq : string;
  pto : string;
  star : string;
  and_ : string;
  emp : string;

  true_ : string;
  false_ : string;

  existential : string;
  qf_separator : string;
  qf_dot : string;

  struct_begin : string;
  struct_end : string;
  struct_separator : string;

  nil : string;

  show_fields : bool;

  print_var : Variable.t -> string;
  print_struct : StructDef.t -> string;
}

let default_printer = {
  eq = "=";
  neq = "!=";
  pto = "->";
  star = "*";
  and_ = "/\\";
  emp = "emp";
  true_ = "true";
  false_ = "false";

  existential = "E";
  qf_separator = ", ";
  qf_dot = ".";

  struct_begin = "<";
  struct_end = ">";
  struct_separator = ", ";

  nil = "nil";

  show_fields = false; (* TODO: true and change convertors *)

  print_var = Variable.show;
  print_struct = StructDef.get_name;
}

let (++) = (^)

let show_list ?(sep=", ") ?(emp="") show = function
  | [] -> emp
  | xs -> String.concat sep @@ List.map show xs

let rec print_term p print_var t = match SL.Term.view t with
  | _ when SL.Term.is_nil t -> p.nil
  | Var v -> print_var v
  | HeapTerm (f, v) -> Format.sprintf "%s[%s]" (Field.show f) (print_term p print_var v)
  | SmtTerm t -> SMT.show t (* TODO *)
  | _ -> failwith @@ SL.Term.show t
  (* TODO: customize *)

let print_atom variadic p phi =
  let var = print_term p p.print_var in
  let show_struct def ys =
    List.map2 (fun field y ->
      let field = if p.show_fields then Format.sprintf "%s: " (Field.show field) else "" in
      Format.sprintf "%s%s" field (var y)
    ) (StructDef.get_fields def) ys
    |> String.concat p.struct_separator
  in
  match SL.view phi with
  | Eq [x1; x2] -> (var x1) ++ p.eq ++ (var x2)
  | Eq xs when variadic -> String.concat p.eq @@ List.map var xs
  | Distinct [x1; x2] -> (var x1) ++ p.neq ++ (var x2)
  | Distinct xs when variadic -> Format.sprintf "distinct(%s)" (String.concat ", " @@ List.map var xs)
  | PointsTo (x, def, ys) ->
    (var x) ++ p.pto ++ (p.print_struct def)
      ++ p.struct_begin ++ show_struct def ys ++ p.struct_end
  | Predicate (name, xs, 0, []) ->
    name ++ "(" ++ show_list var ~sep:p.struct_separator xs ++ ")"
  | Emp -> p.emp
  | _ -> failwith @@ SL.show phi

let print_symbolic_heap ?(precise=true) ?(variadic=false) ?(p=default_printer) phi =
  let print_atom = print_atom variadic p in
  let print_spatial = show_list ~sep:(" " ++ p.star ++ " ") ~emp:p.emp print_atom in
  let print_pure = show_list  ~sep:(" " ++ p.and_ ++ " ") ~emp:p.true_ print_atom in
  (* For s2s, we print spatial part first *)
  if precise && SL.is_symbolic_heap phi then
    let es, atoms = SL.as_quantified_symbolic_heap phi in
    match es with
    | [] -> print_spatial atoms
    | es ->
      Format.asprintf "%s %s%s %s"
        p.existential
        (show_list ~sep:p.qf_separator p.print_var es)
        p.qf_dot
        (print_spatial atoms)
  else if not precise && PreciseToImprecise.is_imprecise_sh phi then
    let es, pure, spatial = PreciseToImprecise.as_imprecise_sh phi in
    match es with
      | [] -> Format.asprintf "%s %s %s" (print_spatial pure) p.and_ (print_pure spatial)
      | es -> Format.asprintf "(%s %s %s %s %s %s)"
                p.existential (show_list ~sep:p.qf_separator p.print_var es) p.qf_dot
                (print_spatial pure) p.and_ (print_pure spatial)
  else failwith ("Not a symbolic heap: " ^ SL.show phi)

let pretty phi =
  let p = {default_printer with
    struct_begin = "{";
    struct_end = "}";
    show_fields = true;
    print_struct = (fun _ -> "");
  }
  in
  print_symbolic_heap ~variadic:true ~p phi
