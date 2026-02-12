let (++) x y = match x, y with
  | "", _ -> y
  | _, "" -> x
  | _, _  -> x ^ "\n" ^ y

let (+++) x y = match x, y with
  | "", _ -> y
  | _, "" -> x
  | _, _  -> x ^ "\n\n" ^ y
