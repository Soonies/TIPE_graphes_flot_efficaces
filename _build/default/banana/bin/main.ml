(*fichier de test et d'execution des programmes*)

module G = Basics.Flow_graph
module M =  Basics.Resolution_strategies
module Rnd =  Random
module D = Basics.Dynamic_array          
         


let f g v = 
  let x,y = G.see_label v in 
 print_string " ( "; print_int x; print_string " , "; print_int y  ; print_string " )   b  = " ; print_int (G.see_supply g v) ; print_newline () 
  
let ff  g e  = 
  let i,j  =  e  in 
  f  g i ; f g  j ; 
  print_string "c  = " ;print_int (G.see_capacity g e) ; print_string "  f  = " ;print_int (G.see_flow g e)  ; print_newline (); print_newline () 

 let a0 = ( 299 , 210 ) 
 let a1 = ( 117 , 74 ) 
 let a2 = ( 264 , 209 ) 
 let a3 = ( 99 , 64 ) 
 let a4 = ( 205 , 46 ) 
 let a5 = ( 275 , 119 ) 
 let a6 = ( 26 , 27 ) 
 let a7 = ( 136 , 295 ) 
 let a8 = ( 18 , 30 ) 
 let a9 = ( 142 , 71 ) 
 let a10 = ( 140 , 261 ) 
 let a11 = ( 16 , 69 ) 
 let a12 = ( 229 , 107 ) 
 let a13 = ( 170 , 42 ) 
 let a14 = ( 83 , 79 ) 
 let a15 = ( 160 , 284 ) 
 let a16 = ( 53 , 289 ) 
 let a17 = ( 224 , 298 ) 
 let a18 = ( 248 , 73 ) 
 let a19 = ( 13 , 228 ) 
 let a20 = ( 246 , 286 ) 
 let a21 = ( 22 , 114 ) 
 let a22 = ( 179 , 297 ) 
 let a23 = ( 273 , 277 ) 
 let a24 = ( 9 , 165 ) 
 let a25 = ( 16 , 145 ) 
 let a26 = ( 217 , 84 ) 
 let a27 = ( 81 , 263 ) 
 let a28 = ( 119 , 161 ) 
 let a29 = ( 178 , 160 ) 
let ls  =  [ a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10; a11; a12; a13; a14; a15; a16; a17; a18; a19; a20; a21; a22; a23; a24; a25; a26; a27; a28; a29; ] 

 let g = G.create ls 
 let ss i x  =    G.fset_supply g i x
  
  let () = 
 ss a0 (2);
 ss a1 (-3);
 ss a2 (-1);
 ss a3 (2);
 ss a4 (-3);
 ss a5 (-1);
 ss a6 (-2);
 ss a7 (2);
 ss a8 (2);
 ss a9 (-2);
 ss a10 (-2);
 ss a11 (-3);
 ss a12 (1);
 ss a13 (3);
 ss a14 (-1);
 ss a15 (-1);
 ss a16 (2);
 ss a17 (2);
 ss a18 (2);
 ss a19 (2);
 ss a20 (1);
 ss a21 (1);
 ss a22 (1);
 ss a23 (-1);
 ss a24 (1);
 ss a25 (1);
 ss a26 (3);
 ss a27 (-2);
 ss a28 (-3);
 ss a29 (-3)


let other  = let new_g = M.min_cost g in 
               new_g  (* M.total 30 200*)

(*let () = G.iter_edg (ff other) other*)

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
                                     
                                     