module type EQUALITY = sig
  type t
  val mk_eq : t list -> t
  val mk_distinct : t list -> t
end
