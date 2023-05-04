(* Simple type checking for SSL parsing
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module M = Stdlib.Map.Make(String)

type t = {
  sorts : Sort.Set.t;   (* Uninterpreted sorts *)
  vars : Sort.t M.t;    (* Declared variables *)

  loc_sort : Sort.t;    (* Sort representing locations *)
  heap_sort : Sort.t;   (* Declared sort of the heap *)
}

exception VariableRedefined of string

exception VariableNotDeclared of string

let empty = {
  sorts = Sort.Set.empty;
  vars = M.empty;

  (* Default SL sorts *)
  loc_sort = Sort.Loc;
  heap_sort = Sort.mk_array Sort.Loc Sort.Loc;
}

let declare_var var sort env =
  if M.mem var env.vars then raise (VariableRedefined var)
  else {env with vars = M.add var sort env.vars}

let type_of_var var env =
  try M.find var env.vars
  with Not_found -> raise (VariableNotDeclared var)

let declare_sort sort env = {env with sorts = Sort.Set.add sort env.sorts}

let declare_loc_sort sort env = {env with loc_sort = sort}

let declare_heap_sort sort env = {env with heap_sort = sort}

let show env =
  let content =
    M.bindings env.vars
    |> List.map (fun (var, sort) -> Format.asprintf "%s : %a" var Sort.pp sort)
    |> String.concat ", "
  in
  "{" ^ content ^ "}"
