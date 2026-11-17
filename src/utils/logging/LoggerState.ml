(* Internal state of logging.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 - 2026 *)

open FileUtils

type query = [`Sat | `Abstraction | `Synthesis] [@@deriving compare]

let show_query = function
  | `Sat -> "query"
  | `Abstraction -> "abstraction"
  | `Synthesis -> "synthesis"

(** Toplevel directory used only when Astral is used as a library *)
let session = ref ""

(** Directory for each check-sat/synthesis command passed to Astral *)
let query = ref ""

(** Number of queries send by user *)
module Queries = Counter.Map (struct
  type t = query
  let compare = compare_query
  let name = "queries"
end)

(** Number of sub-queries performed internally by Astral *)
module SubQueries = Counter.Simple (struct let name = "sub-queries" end)

(** Number of calls to SMT solvers *)
module SMTQueries = Counter.Simple (struct let name = "SMT queries" end)

module Stack = struct
  let self = ref []
  let toplevel = ref 0

  let push q =
    self := (q, 1) :: !self

  let _pop () = match !self with
    | [] -> assert false
    | x :: xs ->
      self := xs;
      x

  let pop () = fst @@ _pop ()

  let top () = List.hd

  let next () = match !self with
    | [] -> toplevel := !toplevel + 1; !toplevel
    | _ ->
    let (name, n) = _pop () in
    self := (name, n + 1) :: !self;
    n + 1

  let get_path () = List.fold_left (fun acc (q, _) -> acc / q) "" !self

end

let init () =
  if Config.Interactive.get () && Config.Debug.get () then (
    session := Config.DebugDir.get ();
    FileUtils.mk_dir_force !session
  )
  else if Config.Debug.get () then (
    query := Config.DebugDir.get ();
    FileUtils.mk_dir_force !query
  )
  else ()

let current_query () = Queries.get `Sat (* TODO *)

(** Start a new query *)
let new_query kind =
  let next = Queries.next kind in
  if Config.Debug.get () then (
    query := Format.asprintf "%s_%04d" (show_query kind) next;
    FileUtils.mk_dir @@ !session / !query
  )

let new_subquery kind =
  SubQueries.increment ();
  let subquery = Format.asprintf "%s_%04d" (show_query kind) (Stack.next ()) in
  Stack.push subquery;
  if Config.Debug.get () then FileUtils.mk_dir_p @@ !session / !query / (Stack.get_path ())

let finish_subquery () =
  ignore @@ Stack.pop ()

let session_dir () = !session
let query_dir () = !session / !query
let current_subquery_dir () = !session / !query / Stack.get_path ()
