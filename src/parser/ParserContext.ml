(* Parser context.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open ParserUtils

module M = Map.Make(String)

type t = {
  (* Declarations *)
  sorts : Sort.Set.t;       (* Uninterpreted sorts *)
  vars : Sort.t M.t;        (* Declared variables *)
  heap_sort : Sort.t list;  (* Declared sort of the heap *)

  (* Attributes *)
  expected_status : [ `Sat | `Unsat | `Unknown of string ];
  attributes : String.t M.t;

  (* Options *)
  produce_models : bool;

  (* Assertions *)
  assertions : SSL.t list;
}

let empty = {
  sorts = Sort.Set.empty;
  vars = M.empty;
  heap_sort = [];

  expected_status = `Unknown "not provided";
  attributes = M.empty;

  produce_models = false;
  assertions = [];
}

(** Declarations *)

let declare_sort ctx sort =
  if Sort.Set.mem sort ctx.sorts then raise @@ SortRedefined sort
  else {ctx with sorts = Sort.Set.add sort ctx.sorts}

let declare_var ctx var sort =
  if M.mem var ctx.vars then raise @@ VariableRedefined var
  else {ctx with vars = M.add var sort ctx.vars}

let type_of_var ctx var =
  try M.find var ctx.vars
  with Not_found -> raise @@ VariableNotDeclared var

let declare_heap_sort ctx sorts = {ctx with heap_sort = sorts}

let set_expected_status ctx = function
  | "sat" -> {ctx with expected_status = `Sat}
  | "unsat" -> {ctx with expected_status = `Unsat}
  | "unknown" -> {ctx with expected_status = `Unknown "not provided"}
  | other -> raise @@ ParserError ("Unknown status '" ^ other ^ "'")

let set_attribute ctx name value = {ctx with attributes = M.add name value ctx.attributes}

let set_produce_models ctx flag = {ctx with produce_models = flag}

let add_assertion ctx phi = {ctx with assertions = phi :: ctx.assertions}

let add_vars ctx vars =
  List.fold_left
    (fun ctx (name, sort) ->
      declare_var ctx name sort
    ) ctx vars

(*** Accessors ***)

let get_vars ctx =
  M.bindings ctx.vars
  |> List.map (fun (name, sort) -> SSL.Variable.mk name sort)

let get_sl_vars ctx = List.filter SSL.Variable.is_loc (get_vars ctx)

let get_phi ctx =
  SSL.mk_and ctx.assertions
  |> SSL.normalise

(*** ==== Pretty-printing ==== *)

let show_sorts ctx =
  Sort.Set.elements ctx.sorts
  |> List.map Sort.show
  |> String.concat ", "
  |> Format.asprintf "Sorts: {%s}"

let show_vars ctx =
  M.bindings ctx.vars
  |> List.map (fun (var, sort) -> Format.asprintf "%s : %s" var (Sort.show sort))
  |> String.concat ", "
  |> Format.asprintf "Variables: {%s}"

let show_attributes ctx =
  M.bindings ctx.attributes
  |> List.map (fun (name, value) -> Format.asprintf "%s = %s" name value)
  |> String.concat ", "
  |> Format.asprintf "Attributes: {%s}"

let show_assertions ctx =
  ctx.assertions
  |> List.map SSL.show
  |> String.concat "\t\n"
  |> Format.asprintf "Assertions: {\t\n%s\n}"

let show_heap_sort ctx =
  List.map Sort.show ctx.heap_sort
  |> String.concat " "

let show ctx =
  Format.asprintf "%s\n%s\n%s\n%s\n%s\n"
    (show_sorts ctx)
    (show_heap_sort ctx)
    (show_vars ctx)
    (show_attributes ctx)
    (show_assertions ctx)
