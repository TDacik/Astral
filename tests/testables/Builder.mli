open Testable_sig

module Make (T : TESTABLE_BASE) : TESTABLE_FULL with type t = T.t
