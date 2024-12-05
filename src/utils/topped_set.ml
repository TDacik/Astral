(*
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module type SET = sig
  include Set.S
  val show : t -> string
end

module Lift (Set : SET) = struct

  exception TopError

  type t =
    | Lifted of Set.t
    | Top

  let show = function
    | Lifted set -> Set.show set
    | Top -> "top"

  include Datatype.Printable(struct
    type nonrec t = t
    let show = show
  end)


  let is_concrete = function
    | Lifted _ -> true
    | _ -> false

  (** Functions to lift set operations. *)

  (** For concrete set apply original operation, raise TopError otherwise. *)
  let lift_or_fail f = function
    | Lifted s -> f s
    | Top -> raise TopError

  let lift_option f = function
    | Lifted s -> Some (f s)
    | Top -> None

  (** Monad interface *)

  let return set = Lifted set

  let (>>=) set f = match set with
    | Lifted s -> Lifted (f s)
    | Top -> Top

  let to_option = function
    | Lifted s -> Some s
    | Top -> None

  (** Set interface *)

  let empty = Lifted Set.empty

  let singleton x = Lifted (Set.singleton x)

  let of_list xs = Lifted (Set.of_list xs)

  let elements = lift_or_fail Set.elements

  let top = Top

  let is_empty = function
    | Lifted s -> Set.is_empty s
    | Top -> false

  let add x s = s >>= Set.add x
  let remove x s = s >>= Set.remove x

  (** Binary operations *)

  let union lhs rhs = match lhs, rhs with
    | Lifted s1, Lifted s2 -> Lifted (Set.union s1 s2)
    | _ -> Top

  let inter lhs rhs = match lhs, rhs with
    | Lifted s1, Lifted s2 -> Lifted (Set.inter s1 s2)
    | Lifted s, Top -> Lifted s
    | Top, Lifted s -> Lifted s
    | Top, Top -> Top

  let diff lhs rhs = match lhs, rhs with
    | Lifted s1, Lifted s2 -> Lifted (Set.diff s1 s2)
    | _, Top -> Lifted Set.empty
    | Top, Lifted _ -> Top (* Approximation *)

  let cartesian_product_aux s1 s2 =
    Set.fold (fun x acc ->
      Set.fold (fun y acc ->
        (x, y) :: acc
      ) s2 acc
    ) s1 []

  let cartesian_product lhs rhs = match lhs, rhs with
    | Lifted s1, Lifted s2 -> Some (cartesian_product_aux s1 s2)
    | _ -> None

  (** Mappings *)

  let map fn set = set >>= (Set.map fn)

  let apply_binop_aux fn s1 s2 =
    Set.fold (fun x acc ->
      Set.fold (fun y acc ->
        match fn x y with
        | Some e -> Set.add e acc
        | None -> acc
      ) s2 acc
    ) s1 Set.empty

  let apply_partial_binop fn lhs rhs = match lhs, rhs with
    | Lifted s1, Lifted s2 -> Lifted (apply_binop_aux fn s1 s2)
    | _ -> Top

  let map2 fn set1 set2 =
    let fn' x1 x2 = Some (fn x1 x2) in
    match set1, set2 with
    | Lifted s1, Lifted s2 -> Lifted (apply_binop_aux fn' s1 s2)
    | _ -> Top

  let apply_partial_variadic_op fn operands =
    if List.for_all is_concrete operands then
      List.map elements operands
      |> BatList.n_cartesian_product
      |> List.map fn
      |> List.filter Option.is_some
      |> List.map Option.get
      |> of_list
    else Top

  let cardinal = lift_or_fail Set.cardinal

  let cardinal_opt = function
    | Lifted x -> Some (Set.cardinal x)
    | Top -> None

  let choose = lift_or_fail Set.choose


  (** Predicates *)

  let equal lhs rhs = match lhs, rhs with
    | Lifted s1, Lifted s2 -> Set.equal s1 s2
    | Top, Top -> true
    | _ -> false

end
