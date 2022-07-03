(* Signatures of first-class modules created by command-line options handling
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module type CONVERTOR = sig

  val name : string

  val convert : SSL.t -> string

  val dump : string -> SSL.t -> unit

end
