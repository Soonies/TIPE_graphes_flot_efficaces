(*convention:                                                 *)
(*          a non existant arc has a capcity of (0,0) and a null cost *)

module G = Basics.Flow_graph

let test ()  = 
  let g = G.create [0;1;2]  in 

  let ea i j c b f = 
    G.fadd_edge g i j ;
    G.fset_capacity  g  i j c;
    G.fset_cost g i j b;
    G.fset_flow_edge g i j f
  in
  let va i s = 
    G.fset_supply g i  s 
  in 

  ea 0 1 (Capa 10) 9 4 ; 
  ea 0 2 (Capa 10) 2 7 ; 
  ea 2 1 (Infty) 1 1 ; 
  ea 1 2 (Capa 2)  10 1 ; 

  va 0 3;
  va 1 4;
  va 2 (-6);


  g

let g =test ()  

module  X :Basics.Display.Graph  = struct
  type a = int
  let input_g = g 
  let string_of_a = (*fun (x,y) ->  (string_of_int x ^ string_of_int y) *)
    string_of_int
end

module Dis1  =  Basics.Display.DisplayFunct (X) 
let () = Dis1.generate "flow_geraph11.dot" 
let () = Dis1.convert_dot_to_png "flow_geraph11.dot" "flowegrapheu11.png"


(*

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
let () = Dis2.make_image () *)