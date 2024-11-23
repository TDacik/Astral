(* Arithmetic on integer intervals.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

type t = Int.t * Int.t [@@deriving compare, equal]

let show (x, y) = Format.asprintf "[%d, %d]" x y

include Datatype.Printable(struct
  type nonrec t = t
  let show = show
end)

let monus x y = max 0 (x - y)

let plus (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

let minus (x1, x2) (y1, y2) = (monus x1 y1, monus x2 y2)

let join (x1, x2) (y1, y2) = (max x1 y1, min x2 y2)

let meet (x1, x2) (y1, y2) = (min x1 y2, max x2 y2)

let min m (x1, x2) = (min m x1, min m x2)

let max m (x1, x2) = (max m x1, max m x2)

let meet_list (x :: xs) =
  let rec inner xs = match xs with
    | [] -> x
    | x :: xs -> meet x (inner xs)
  in
  inner (x :: xs)

let enum (x1, x2) = BatList.range x1 `To x2
