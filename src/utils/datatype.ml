(* Smart datatypes
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Datatype_sig

module Printable (M : SHOWABLE) = struct

  include M

  let pp fmt x = Format.fprintf fmt "%s" (M.show x)

end

module Collections (M : COMPARABLE) = struct

  type t = M.t [@@ ocaml.warning "-34"]

  module Set = struct

    include Set.Make(M)

    let show = BatSet.print ~first:"{" ~sep:"," ~last:"}"

  end

  module Map = Map.Make(M)

end
