(*convention:                                                 *)
(*          a non existant arc has a capcity of (0,0) and a null cost *)

module G = Basics.Flow_graph
module M =  Basics.Resolution_strategies
module Rnd =  Random
module D = Basics.Dynamic_array






let a, b , c ,d= (100,100),(200,75),(250,200), (50,75)
 let ls  =  [a;b;c;d]
let g = G.create ls

let ss i x  = 
  G.fset_supply g i x

let h  i  j  u c  =
  G.fset_capacity g i j u;
  G.fset_cost g i j c

let () =  ss a (5);
          ss b (-1);
          ss c (10);
          ss d  (-14);
          M.complete_graph_of_graph g;
          M.fill_cost g 
          
         


let g'  = M.to_no_parallel_edges g

let g'' = M.to_s_source_s_sink g'



let rg  = M.make_residual g''


let mff v = match G.see_label v  with
  | M.N x -> let i,j  =  x in 
           print_string " ( "; print_int i; print_string " , "; print_int j  ; print_string " )  "
  | M.C(a,b) -> let a1, a2  =  a in 
              let b1, b2 = b  in
              print_string "C(" ;
                print_int a1; print_string " , "; print_int a2   ; print_string  " | ";  
                print_int b1; print_string " , "; print_int b2;
              print_string ") " 

let mfff  g e = 
  let i,j  =  e  in 
  mff i ; mff j ;print_string "c  = " ; print_int (G.see_capacity g e) ; print_string "  f  = " ; print_int (G.see_flow g e) ; print_newline (); print_newline ()
  

let f g v = 
  let x,y = G.see_label v in 
 print_string " ( "; print_int x; print_string " , "; print_int y  ; print_string " )   b  = " ; print_int (G.see_supply g v) ; print_newline () 
  
let ff  g e  = 
  let i,j  =  e  in 
  f  g i ; f g  j ; 
  print_string "c  = " ;print_int (G.see_capacity g e) ; print_string "  f  = " ;print_int (G.see_flow g e)  ; print_newline (); print_newline () 

let other  =  M.total 4



(**)

(*
let newi  =  M.from_no_parallel_edges g''.graph 
*)

(*
module  X :Basics.Display.Graph  = struct
  type a = int*int
  let input_g = g 
  let string_of_a = fun (x,y) ->  (string_of_int x ^ string_of_int y)
end

module Dis1  =  Basics.Display.DisplayFunct (X) 

*)

module  X' :Basics.Display.TempGraph  = struct 
  let input_g = other
  let string_of_a = fun (x,y) ->  (string_of_int x ^ string_of_int y)
end



module Dis2  = Basics.Display.DisplayPlane (X')
(* let () = Dis1.generate "flow_geraph8.dot" 
let () = Dis1.convert_dot_to_png "flow_geraph8.dot" "flowegrapheu8.png"
*)

(*fonction pr print les attrivuts des vertex*)
                                    
                                    
  (*                                 
let test_succ i  =                   
  print_string "pr le vertex: ";  f i ; 
  let voisins =  G.succ_ver g''' i  in
  G.iter_vset (f) voisins            
                                     
let ()  =  G.iter_ver test_succ g''' 
 let () = G.iter_edg ff g'''         
                                     
                                    
 *)                                  
                                     
let () = Dis2.make_image ()          
                                     
                                     