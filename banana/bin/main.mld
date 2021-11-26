(*convention:                                                 *)
(*          a non existant arc has a capcity of 0 and a null cost *)

module G = Basics.Flow_graph
module Helper  = Basics.Helper
module Std_alg = Basics.Std_algorithms
exception Infeasible_flow
open G 
open Helper

(*
let cycle_canceling g0 =

  let g = sing_source_sing_sink g0 in
  let g' = Std_alg.maximum_flow g in

  if g' = empty then
  raise Infeasible_flow
  else
  let r  = ref (to_residual_graph g') in 
  let c = ref (Std_alg.neg_cycle !r) in 
  while !c <> [] do
    let w = vpath_to_epath (!c) in 
    let rcapacities_w = List.map ( see_capacity !r ) w in 
    let min_w = List.fold_left min_capa Infty rcapacities_w in 
    (*/!\ erreur ptet*)
    (*since we assumed that G contains no uncapa neg cost cycle *)
    let delta =   match min_w with
                    | Infty -> raise Uncapacitated_negcost_cycle
                    | Capa x -> x 
    in
    do_along_epath (incr_fl_along_edge g' delta) w;
    r := to_residual_graph g';
    c := Std_alg.neg_cycle !r

  done ;
  g'
   *)






let test ()  = 
  let g = G.create 4  in 

  G.add_edge g (0,2); 
  G.add_edge g (0,1);
  G.add_edge g (1,2); 
  G.add_edge g (2,3);
  G.add_edge g (1,3);
  
  G.set_flow g (0,2) 3;
  G.set_flow g (0,1) 2;
  G.set_flow g (1,2) 2;
  G.set_flow g (2,3) 5;
  G.set_flow g (1,3) 0;

  G.set_capacity g (0,2) (Capa 4);
  G.set_capacity g (0,1) (Capa 2);
  G.set_capacity g (1,2) (Capa 3);
  G.set_capacity g (2,3) (Capa 5);
  G.set_capacity g (1,3) (Capa 1);

  G.set_cost g (0,2) 3;
  G.set_cost g (0,1) 2;
  G.set_cost g (1,2) 2;
  G.set_cost g (2,3) 5;
  G.set_cost g (1,3) 0;

  G.set_supply g 1 9;
  
  let _ = read_int () in 
  
  g

let g =test () 
 let g'= G.sing_source_sing_sink g 

module  X :Basics.Display.X_graph  = struct let input_g = g' end
module Dis  =  Basics.Display.DisplayFunct (X) 

let () = Dis.generate "flow_geraph.dot" 
let () = Dis.convert_dot_to_png "flow_geraph.dot" "flowegrapheu.png"