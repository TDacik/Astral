(* Generation of random formulae
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module M1 = Generator.Make
  (struct
    let n_vars = 5
    let depth = 4
    let lists = false
    let unfold = true
  end)

let run n = M1.generate n (Options.quickcheck_store ()) "test"
