(* Smart datatypes
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Datatype_sig

module Printable (M : SHOWABLE) : PRINTABLE with type t := M.t
(** Builds pretty-printing functions from a show function *)

module Collections (M : COMPARABLE) : COLLECTIONS with type t := M.t
(** Builds set and map over type t *)
