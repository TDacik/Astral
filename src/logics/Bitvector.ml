(* Utilities for manipulation with vectors of bits.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

exception OutOfBounds

(** Internally, a bitvector is represented as an integer which it represents together with
    a integer width that is used to represent padding zeros. *)
type t = Int.t * Int.t
  [@@deriving compare, equal]

let to_int = fst
let width = snd

(* ==== Constructors ==== *)

let of_int n width = (n, width)

let zero width = (0, width)
let one width = (1, width)

let full_zeros width = (0, width)
let full_ones width = ((BatInt.pow 2 width) - 1, width)

(* ==== Operations over bitvectors ==== *)

let nth (bv, width) index =
  if index >= width then raise OutOfBounds
  else Int.logand 1 @@ Int.shift_right bv index = 1

(* ==== Conversion from SMT string representation ==== *)

let of_string str =
  let _, str = BatString.replace ~str ~sub:"#" ~by:"0" in
  let width_factor = match BatString.get str 1 with
    | 'b' | 'B' -> 1
    | 'x' | 'X' -> 4
    | _ -> Utils.internal_error ("Bitvector.of_string " ^ str)
  in
  (int_of_string str, width_factor * (String.length str - 2))

(* ==== Conversion to bitstring ==== *)

let nth_bit x n =
  if x land (1 lsl n) <> 0
  then '1'
  else '0'

let to_bits (x, width) = BatString.rev @@ String.init width (nth_bit x)

let to_string bv = "#b" ^ to_bits bv

(* ==== Conversion to set ==== *)

let to_set bv =
  let _, width = bv in
  (* Right fold ensures that the list is ascending. *)
  BatString.fold_righti
    (fun i c acc -> match c with
      | '1' -> (i, width) :: acc
      | '0' -> acc
    ) (BatString.rev @@ to_bits bv) []

let show = to_string

module Self = struct
  type nonrec t = t
  let show = show
  let equal = equal
  let compare = compare
end

include Datatype.Printable(Self)
include Datatype.Collections(Self)
