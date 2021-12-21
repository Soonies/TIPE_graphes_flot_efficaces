(*convention:                                                 *)
(*          a non existant arc has a capcity of 0 and a null cost *)

module G = Basics.Flow_graph

let test ()  = 
  let g = G.create [0;1;2;3]  in 

  G.fadd_edge g 0 2 ; 
  G.fadd_edge g 0 1 ;
  G.fadd_edge g 1 2 ; 
  G.fadd_edge g 2 3 ;
  G.fadd_edge g 1 3 ;
  
  G.fset_flow_edge g 0 2 3;
  G.fset_flow_edge g 0 1 2;
  G.fset_flow_edge g 1 2 2;
  G.fset_flow_edge g 2 3 5;
  G.fset_flow_edge g 1 3 0;

  G.fset_capacity g 0 2 (Capa 4);
  G.fset_capacity g 0 1 (Capa 2);
  G.fset_capacity g 1 2 (Capa 3);
  G.fset_capacity g 2 3 (Capa 5);
  G.fset_capacity g 1 3 (Capa 1);

  G.fset_cost g 0 2 3;
  G.fset_cost g 0 1 2;
  G.fset_cost g 1 2 2;
  G.fset_cost g 2 3 5;
  G.fset_cost g 1 3 0;

  G.fset_supply g 1 9;
  
  g

let g =test ()  

module  X :Basics.Display.X_graph  = struct let input_g = g end
module Dis  =  Basics.Display.DisplayFunct (X) 



let () = Dis.generate "flow_geraph11.dot" 
let () = Dis.convert_dot_to_png "flow_geraph11.dot" "flowegrapheu11.png"