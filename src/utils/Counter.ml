(* Simple imperative counter.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module Simple () = struct
  let cnt = ref 0
  let inc () = cnt := !cnt + 1
  let dec () = cnt := !cnt - 1
  let get () = !cnt
  let next () = inc (); get ()
end
