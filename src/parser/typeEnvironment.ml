(* Simple type checking for SSL parsing
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module M = Stdlib.Map.Make(String)

type t = Sort.t M.t

exception VariableRedefined of string

exception VariableNotDeclared of string

let empty = M.empty

let declare env var sort =
  if M.mem var env then raise (VariableRedefined var)
  else M.add var sort env

let type_of env var =
  try M.find var env
  with Not_found -> raise (VariableNotDeclared var)

let show env =
  let content =
    M.bindings env
    |> List.map (fun (var, sort) -> Format.asprintf "%s : %a" var Sort.pp sort)
    |> String.concat ", "
  in
  "{" ^ content ^ "}"
