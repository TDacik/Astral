open Logic_sig

type t =
  | Var of SSL.Variable.t     (* Location variable *)
  | Pure of SMT.Term.t        (* Pure boolean term which does not contain location variables *)

  (* Atoms *)
  | Eq of t list
  | Distinct of t list
  | PointsTo of t * t list
  | LS of t * t
  | DLS of t * t * t * t
  | NLS of t * t * t
  | SkipList of int * t * t

  (* Boolean connectives *)
  | And of t * t
  | Or of t * t
  | Not of t
  | GuardedNeg of t * t

  (* Quantifiers *)
  | Exists of t list * t
  | Forall of t list * t

  (* Spatial connectives *)
  | Star of t list
  | Septraction of t * t

  (* Syntactic sugar *)
  | True
  | False
  | Emp
  | Implies of t * t
  | Iff of t * t

include LOGIC with type t := t

val of_ssl : SSL.t -> t

val to_ssl : t -> SSL.t
