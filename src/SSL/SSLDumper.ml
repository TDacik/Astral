(* Output to smtlib format.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open SSL.Struct

module F = Format

(* === Indentation managment === *)

let indent_increment = 2

let current_indent = ref 0

let increase_indent () = current_indent := !current_indent + indent_increment

let decrease_indent () = current_indent := !current_indent - indent_increment

let indent () = BatString.make !current_indent ' '

(* === Printer === *)

let header = (* TODO: typing *)
"; Generated by Astral\n
(set-logic QF_ALL)
(declare-sort LS_t 0)
(declare-heap (LS_t LS_t))
"

let declare_var var =
  if SSL.Variable.is_nil var then ""
  else
    Format.asprintf "(declare-const %a %a)"
      SSL.Variable.pp var
      Sort.pp @@ SSL.Variable.get_sort var



let translate_vars phi =
  SSL.get_vars phi
  |> List.filter (fun v -> not @@ SSL.Variable.is_nil v)
  |> List.map declare_var
  |> String.concat "\n"

let convert_binders xs =
    List.map
      (fun x -> match x with
        | SSL.Var (name, sort) -> F.asprintf "(%s %a)" (Identifier.show name) Sort.pp sort
        | _ -> assert false
      ) xs
    |> String.concat " "

let pretty_atom = function
  | LS (v1, v2) -> Format.asprintf "(ls %s %s)" (SSL.show v1) (SSL.show v2)
  | DLS (x, y, f, l) -> Format.asprintf "(dls %s %s %s %s)"
    (SSL.show x) (SSL.show y) (SSL.show f) (SSL.show l)
  | NLS (x, y, z) -> Format.asprintf "(nls %s %s %s)"
    (SSL.show x) (SSL.show y) (SSL.show z)
  | PointsTo (x, LS_t y) -> Format.asprintf "(pto %s %s)" (SSL.show x) (SSL.Variable.show y)
  | PointsTo (x, DLS_t (n, p)) -> Format.asprintf "(pto %s (c_dls %s %s))"
    (SSL.show x) (SSL.Variable.show n) (SSL.Variable.show p)
  | PointsTo (x, NLS_t (t, n)) -> Format.asprintf "(pto %s (c_nls %s %s))"
    (SSL.show x) (SSL.Variable.show t) (SSL.Variable.show n)

  | Eq [v1; v2] -> Format.asprintf "(= %s %s)" (SSL.show v1) (SSL.show v2)
  | Distinct [v1; v2] -> Format.asprintf "(distinct %s %s)" (SSL.show v1) (SSL.show v2)

let rec pretty phi =
  if SSL.is_atom phi then () else increase_indent ();
  let res = match phi with
  | Emp -> "emp"
  | And (f1, f2) -> F.asprintf "(and %s %s" (pretty f1) (pretty f2)
  | Or (f1, f2) -> F.asprintf "(or %s %s" (pretty f1) (pretty f2)
  | Not f -> F.asprintf "(not %s" (pretty f)
  | GuardedNeg (f1, f2) -> F.asprintf "(and %s (not %s)" (pretty f1) (pretty f2)
  | Star fs -> F.asprintf "(sep %s" (String.concat " " @@ List.map pretty fs)
  | Septraction (f1, f2) -> F.asprintf "(not (wand %s (not %s))"
      (pretty f1) (pretty f2)
  | Exists (xs, psi) -> F.asprintf "(exists (%s) %s" (convert_binders xs) (pretty psi)
  | _ -> pretty_atom phi
  in
  if SSL.is_atom phi
  then begin
    res
  end
  else begin
    let _indent = indent () in
    decrease_indent ();
    "\n" ^ _indent ^ res ^ "\n" ^ (indent ()) ^ ")\n"
  end

let pretty phi = match phi with
  | GuardedNeg (lhs, rhs) ->
    Format.asprintf "(assert %s)\n\n(assert (not %s))" (pretty lhs) (pretty rhs)
  | phi -> Format.asprintf "(assert %s)" (pretty phi)

let translate_all phi =
  header
  ^ "\n"
  ^ (translate_vars phi)
  ^ "\n\n"
  ^ (pretty phi)
  ^ "\n\n"
  ^ "(check-sat)"

let dump file phi status =
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  Printf.fprintf channel "(set-option :status %s)\n\n" status;
  Printf.fprintf channel "%s\n" (translate_all phi);
  close_out channel
