(* Simple type checking for SSL parsing
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL

module M = Stdlib.Map.Make(String)

let env = ref (M.empty : Sort.t M.t)

let declare var sort =
  try
    let _ = M.find var !env in
    failwith (Format.asprintf "Variable %s redefined" var)
  with Not_found ->
    env := M.add var sort !env

let type_of var =
  try M.find var !env
  with Not_found -> failwith (Format.asprintf "Variable %s is not declared" var)

let show () =
  let content =
    M.bindings !env
    |> List.map (fun (var, sort) -> Format.asprintf "%s :%a" var SSL.Sort.pp sort)
    |> String.concat " "
  in
  "{" ^ content ^ "}"
