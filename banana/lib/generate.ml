module G  = Flow_graph
module Rnd = Random
module P = Plplot.Plot
let square_dim  = 300

let generate_instance n  = 
  let () = Rnd.self_init () in 
  let f  =  Rnd.int in 
  let s  =  square_dim in 

  let rec gen_lst compt accu  = match compt with
    | 0 -> accu 
    | x -> gen_lst (x-1) ((f s , f s):: accu)
  in
  G.create (gen_lst n [])

