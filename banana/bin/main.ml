(*convention:                                                 *)
(*          a non existant arc has a capcity of (0,0) and a null cost *)

module G = Basics.Flow_graph

let test ()  = 
  let g = G.create [(0,0);(3,1);(2,6);(3,3)]  in 

  G.fadd_edge g (0,0) (2,6) ; 
  G.fadd_edge g (0,0) (3,1) ;
  G.fadd_edge g (3,1) (2,6) ; 
  G.fadd_edge g (2,6) (3,3) ;
  G.fadd_edge g (3,1) (3,3) ;
  
  G.fset_flow_edge g (0,0) (2,6) 3;
  G.fset_flow_edge g (0,0) (3,1) 2;
  G.fset_flow_edge g (3,1) (2,6) 2;
  G.fset_flow_edge g (2,6) (3,3) 5;
  G.fset_flow_edge g (3,1) (3,3) 0;

  G.fset_capacity g (0,0) (2,6) (Capa 4);
  G.fset_capacity g (0,0) (3,1) (Capa 2);
  G.fset_capacity g (3,1) (2,6) (Capa 3);
  G.fset_capacity g (2,6) (3,3) (Capa 5);
  G.fset_capacity g (3,1) (3,3) (Capa 1);

  G.fset_cost g (0,0) (2,6) 3;
  G.fset_cost g (0,0) (3,1) 2;
  G.fset_cost g (3,1) (2,6) 2;
  G.fset_cost g (2,6) (3,3) 5;
  G.fset_cost g (3,1) (3,3) 0;

  G.fset_supply g (3,1) 9;
  G.fset_supply g (2,6) 4;
  g

let g =test ()  

module  X :Basics.Display.Graph  = struct
  type a = int*int
  let input_g = g 
  let string_of_a = fun (x,y) ->  (string_of_int x ^ string_of_int y)
end

module Dis1  =  Basics.Display.DisplayFunct (X) 
let () = Dis1.generate "flow_geraph11.dot" 
let () = Dis1.convert_dot_to_png "flow_geraph11.dot" "flowegrapheu11.png"


module  X' :Basics.Display.TempGraph  = struct 
  let input_g = g 
  let string_of_a = fun (x,y) ->  (string_of_int x ^ string_of_int y)
end
module Dis2  = Basics.Display.DisplayPlane (X')


(*
let f v = 
  let x,y = G.see_label v in 
  print_int x;  print_int y ; print_newline ()

let ff e  = 
  let i,j  =  e  in 
  f i ; f j ; 
  print_int (G.see_cost g e) ; print_int (G.see_flow g e)

let () = G.iter_edg ff g

*)
let () = Dis2.make_image ()