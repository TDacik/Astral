open Logic_sig

module Make (Term : TERM) : LOGIC with type t := Term.t
