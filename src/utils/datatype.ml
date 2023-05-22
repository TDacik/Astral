(* Smart datatypes
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Datatype_sig

module Printable (M : SHOW) = struct

  include M

  let pp fmt x = Format.fprintf fmt "%s" (M.show x)

  let print x = Format.printf "%s\n" (M.show x)

  let dump filename x =
    let channel = open_out_gen [Open_creat; Open_wronly] 0o666 filename in
    Printf.fprintf channel "%s\n" (M.show x);
    close_out channel

end

module Comparable (M : COMPARISON) = struct

  include M

  let equal lhs rhs = compare lhs rhs == 0

end

module Collections (M : COMPARISON) = struct

  type t = M.t [@@ ocaml.warning "-34"]

  module Set = struct

    include Set.Make(M)

    let show = BatSet.print ~first:"{" ~sep:"," ~last:"}"

  end

  module Map = Map.Make(M)

end
