(* List of available backends with their metadata.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

type kind =
  | Native of string   (* name of opam package *)
  | External of string (* name of binary *)

type backend_info = {
  name : string;
  kind : kind;
  description : string;
  check_available : unit -> bool;
}

let is_native b = match b.kind with Native _ -> true | _ -> false

let backends = ref []

let register_native ?(description="") ~package ~available name =
  let backend = {
    name = name;
    kind = Native package;
    description = description;
    check_available = (fun () -> available);
  }
  in
  backends := backend :: !backends

let register_external ?(description="") ~binary name =
  let backend = {
    name = name;
    kind = External binary;
    description = description;
    check_available = (fun () -> BackendUtils.binary_exists binary);
  }
  in
  backends := backend :: !backends

let print_aux cond b =
  let status b =
    if (b.check_available ()) then "available"
    else match b.kind with
      | Native package -> Format.asprintf "- (install opam package %s)" package
      | External binary -> Format.asprintf "- (add binary %s to path)" binary
  in
  if cond then Format.printf "  - %s: %s\n" b.name (status b) else ()

let print () =
  Format.printf "Native backends:\n";
  List.iter (fun b -> print_aux (is_native b) b) !backends;
  Format.printf "External backends:\n";
  List.iter (fun b -> print_aux (not @@ is_native b) b) !backends
