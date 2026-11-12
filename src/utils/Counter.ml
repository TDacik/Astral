(* Simple imperative counter.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

type getter = {
  name : string;
  get : unit -> int
}

let registry : getter list ref = ref []

let register name ref =
  registry := {name; get = (fun () -> !ref)} :: !registry

let print () =
  List.iter (fun getter ->
    Format.printf "%s: %d\n" getter.name (getter.get ())
  ) !registry

module type CONFIG = sig
  val name : string
end

module type CONFIG_MAP = sig
  type t
  val compare : t -> t -> int
  val name : string
end

module Simple (C : CONFIG) = struct
  let cnt = ref 0
  let () = register C.name cnt

  let increment () = cnt := !cnt + 1
  let decrement () = cnt := !cnt - 1
  let get () = !cnt
  let next () = increment (); get ()

  let equals n = Int.equal n @@ get ()
end

module Map (C : CONFIG_MAP) = struct
  module M = Map.Make(C)

  let cnt = ref M.empty
  let register () = () (* TODO *)

  let increment x = cnt := M.update x (function None -> Some 1 | Some n -> Some (n+1)) !cnt
  let decrement x = cnt := M.update x (function None -> Some (-1) | Some n -> Some (n-1)) !cnt
  let get x = Option.value ~default:0 @@ M.find_opt x !cnt
  let next x = increment x; get x

  let equals n x = Int.equal n @@ get x
end
