(* Representation of internal exceptions used by solver.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

(** Fatal exceptions. *)

exception UserError of string * string

exception InternalError of Printexc.raw_backtrace * string * string

exception NotImplemented of string
 [@@warning "-38"]

(** Internal exceptions. *)

exception UnknownResult of string * string

exception UnsupportedFragment of string * string


let unsupported_fragment ?(details="") =
  Format.kasprintf (fun s -> raise @@ UnsupportedFragment (s, details))

let unknown_result ?(details="") =
  Format.kasprintf (fun s -> raise @@ UnknownResult (s, details))

let internal_error ?(details="") =
  let trace = Printexc.get_callstack 1000 in
  Format.kasprintf (fun s -> raise @@ InternalError (trace, s, details))

let user_error ?(details="") =
  Format.kasprintf (fun s -> raise @@ UserError (s, details))
