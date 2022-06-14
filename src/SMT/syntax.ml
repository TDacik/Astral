module rec Sort = struct

  type t =
    | Integer
    | Enumeration of string * string list
    | Set of t
    | Array of t * t

  (* == Enumeration sort == *)

  let mk_enum_sort name constants = Enumeration (name, constants)

  let get_constants = function
    | Enumeration (_, consts) -> List.map (fun c -> Term.Constant c) consts

end

and Term = struct

  type t =
    | Constant of String.t
    | Variable of String.t * Sort.t
    (* LIA *)
    | Plus of t * t
    | Mult of t * t
    (* Sets *)
    | Union of t * t
    | Inter of t * t
    | Diff of t * t
    | Compl of t

    (* Arrays *)
    | Select of t * t
    | Store of t * t * t

    (* Boolean *)
    | And of t * t
    | Or of t * t
    | Not of t * t
    | Implies of t * t
    | Iff of t * t
    | True
    | False

  (* ==== Constructors ==== *)

  let mk_false () = False
  let mk_true () = True
  let mk_and t1 t2 = And (t1, t2)
  let mk_or t1 t2 = Or (t1, t2)
  let mk_not t = Not t

  let mk_plus t1 t2 = Plus (t1, t2)
  let mk_mult t1 t2 = Mult (t1, t2)

  let mk_union t1 t2 = Union (t1, t2)
  let mk_inter t1 t2 = Inter (t1, t2)
  let mk_diff t1 t2 = Diff (t1, t2)
  let mk_compl t = Compl t

end
