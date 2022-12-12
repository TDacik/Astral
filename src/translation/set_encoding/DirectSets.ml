(* Direct encoding of sets
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

include SMT.Set

let name = "direct"

let rewrite term = term

let rewrite_back _ model = model
