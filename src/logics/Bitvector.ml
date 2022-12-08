(* Utilities for manipulation with vectors of bits.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

(** Internally, a bitvector is represented as an integer which it represents together with
    a integer width that is used to represent padding zeros. *)
type t = Int.t * Int.t
  [@@deriving compare, equal]

let to_int = fst
let width = snd

let show (x, width) = Format.asprintf "(%d, %d)" x width

module Self = struct
  type nonrec t = t
  let show = show
  let equal = equal
  let compare = compare
end

include Datatype.Printable(Self)
include Datatype.Collections(Self)

(* ==== Constructors ==== *)

let of_int n width = (n, width)

let zero width = (0, width)
let one width = (1, width)

let full_zeros width = (0, width)
let full_ones width = ((BatInt.pow 2 width) - 1, width)

let%test _ = full_zeros 1 = (0, 1)
let%test _ = full_zeros 4 = (0, 4)

let%test _ = full_ones 1 = (1, 1)
let%test _ = full_ones 4 = (15, 4)

(* ==== Conversion from SMT string representation ==== *)

let of_string str =
  let _, str = BatString.replace ~str ~sub:"#" ~by:"0" in
  let width_factor = match BatString.get str 1 with
    | 'b' | 'B' -> 1
    | 'x' | 'X' -> 4
  in
  (int_of_string str, width_factor * (String.length str - 2))

let%test _ = of_string "#B000" = (0, 3)
let%test _ = of_string "#X000" = (0, 12)

let%test _ = of_string "#b11" = (3, 2)
let%test _ = of_string "#x2a" = (42, 8)

(* ==== Conversion to bitstring ==== *)

let nth_bit x n =
  if x land (1 lsl n) <> 0
  then '1'
  else '0'

let to_bits (x, width) = String.init width (nth_bit x)

let to_string bv = "#b" ^ to_bits bv

let%test _ = to_string (0, 3) = "#b000"
let%test _ = to_string (3, 2) = "#b11"

(* ==== Conversion to set ==== *)

let to_set bv =
  let _, width = bv in
  (* Right fold ensures that the list is ascending. *)
  BatString.fold_righti
    (fun i c acc -> match c with
      | '1' -> (i, width) :: acc
      | '0' -> acc
    ) (to_bits bv) []

let%test _ = to_set (0, 3) = []
let%test _ = to_set (3, 2) = [(0, 2); (1, 2)]
let%test _ = to_set (5, 4) = [(0, 4); (2, 4)]
