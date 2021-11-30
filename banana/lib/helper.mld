module G  =  Flow_graph
open G

(*capa help*)
let min_capa c1 c2 = match c1 with
  | Infty -> c2
  | Capa x -> match c2 with 
              | Infty -> Infty
              | Capa y -> Capa (min x y) 

(* path help*)
let vpath_to_epath w  = 
  let rec aux ls accu = match ls with
    | _ :: [] | [] -> accu
    | i :: j :: tl -> aux (j :: tl) ( (i,j)::accu )
  in
  aux w []


let  rec do_along_epath f (p:epath) = match p with
  | [] -> ()
  | hd :: tl -> f hd;
                do_along_epath f tl

                
(*flow help*)
let has_licit_flow g = 

  let mass_balance (v:G.vertex) = 
    let prede_v = prede_ver g v in
    let succ_v = succ_ver g v in 

  (*count in and out flow*)
    let in_flow = List.fold_left (fun c_in_flow x-> c_in_flow +  see_flow g (x,v) ) 0 prede_v in 
    let out_flow = List.fold_left (fun c_out_flow x-> c_out_flow +  see_flow g (v,x) ) 0 succ_v in
    
    out_flow - in_flow = see_supply g v
  in

  let nodes_balanced  = ref true  in
  iter_ver (fun x -> nodes_balanced := !nodes_balanced && mass_balance x) g ;
  
  !nodes_balanced

let incr_fl_along_edge g delta (i,j) =  
  let new_flow = see_flow g (i,j) + delta   in 
    set_flow  g (i,j) new_flow

(*residual graph help*)
let to_residual_graph g =
  let n  = size g in 
  let r = create n in
  set_residuality r true;

  (*setting costs and residual capacities in a specif arc*)
  
  let make_resi_arcs (i,j) = 
      
    let x_ij = see_flow g (i,j) in        
    let c_ij = see_cost g (i,j) in   
    let u_ij = see_capacity g (i,j) in 

    let r_ij = u_ij -- Capa x_ij   in
    
    if r_ij >> Capa 0 then 
      ( add_fedge r (i,j);
        set_cost r (i,j) c_ij ;
        set_capacity r (i,j) r_ij) ;
    
    if x_ij > 0 then 
      ( add_bedge r (j,i);
        set_cost r (j,i) (- c_ij);
        set_capacity r (j,i) (Capa x_ij) );

  in
    (*iters through all edges and create the corresponding arc in the residual graph *)
  iter_edg make_resi_arcs g;
  r
exception Uncapacitated_arc_found of edge
exception Zero_cost_arc_found of edge
let from_residual_graph r =  (* no uncapacitated arc*)
  
  if  not ( is_residual r ) then raise ( Not_residual_graph r ) ;  
  let n  = size r in 
  let g  =  create n in
  let seen  =  Hashtbl.create (n*n)  in 

  let capa_to_int e = function
    | Infty -> raise (Uncapacitated_arc_found e)
    | Capa x  -> x 
  in 

  (*setting cost and capacities in g graph*)
  let make_arc ((i,j):edge) = 
    if not ( Hashtbl.mem seen (i,j) ||  Hashtbl.mem seen (j,i) ) then

      let r_ij = see_capacity r (i,j)   in 
      let r_ji = see_capacity r (j,i)   in (* should be 0 default if edge doesnt exist  *)
      let rc_ij = see_cost r (i,j)      in 
      let rc_ji = see_cost r (j,i)      in 
    
      if r_ij <>Capa 0 || r_ji <> Capa 0 then
      
        ( let f_e  = match e_type r (i,j) with
              | Forward ->  (i,j) 
              | Backward -> (j,i)
        in 
          let b_e = match f_e with 
                  | (a,b) -> (b,a)
        in
        
        let u = r_ij ++ r_ji           in
        let x  =  capa_to_int b_e (see_capacity r b_e) in 
        let c =    (max ( abs rc_ij ) ( abs rc_ji ) ) in
  
        add_edge g f_e;
        set_capacity g f_e u;
        set_cost g f_e c ;
        set_flow g f_e x ) 
      ;
    
      Hashtbl.add seen (i,j) true ; 
      Hashtbl.add seen (j,i) true 
  in
  iter_edg make_arc r;
  g 

let convert_flow_to_residual g r flow0=
  let make_resi_flow (i,j) = 
    let x_ij = see_flow g (i,j) in  
    let x_ij0 = flow0 (i,j) in
    
    let x_ij' = if x_ij > x_ij0 then x_ij - x_ij0 else 0 in
    let x_ji' = if x_ij > x_ij0 then 0 else x_ij0 - x_ij  in
      
    set_flow r (i,j) x_ij';
    set_flow r (j,i) x_ji';
  in

  iter_edg make_resi_flow g

let convert_flow_from_residual g r flow0 =

  let make_norm_flow (i,j) = 
    let x_ij' = see_flow r (i,j) in
    let x_ji' =  see_flow r (j,i) in 
    let x_ij0 = flow0 (i,j) in

    let x_ij = x_ij' - x_ji' + x_ij0 in 

    set_flow g (i,j) x_ij
  in

  iter_edg make_norm_flow g

                    
(**)
let creation_wizard () = 
let  g = ref empty in 

let () = print_string "Creation Wizard. \n
                        Enter graph size  \n" in
let  n = read_int () in 
g := create n ;

for i = 0 to  n-1 do 
  let () = print_endline ("\n Set supply for vertex " ^ string_of_int i ^ " : ") in 
  let b = read_int () in 
  set_supply !g i b;
  print_newline () 
done;
let () = print_endline "\n add an edge or dismiss (e/d) \n " in
let x  =  ref (read_line ()) in  

while !x <> "d" do  
  let () = print_endline "\n Tail: " in 
  let t = read_int () in 
  let () = print_string " Head : " in 
  let h  = read_int () in 
  let () = print_string " Capa : " in 
  let cap  = read_int () in  
  let () = print_string " cost : " in 
  let cost  = read_int () in
  let () = print_string " flow : " in 
  let flo  = read_int () in
  
  let e = (t,h) in 
  add_edge !g e;
  set_capacity !g e (Capa cap) ; 
  set_cost !g e cost ; 
  set_flow !g e flo;

  let () = print_endline "\n add an edge or dismiss (e/d) \n " in
  x :=  (read_line ())
done ; 

let () = print_endline "\n Graph defined." in 
let is_ok = has_licit_flow !g in 
let () =  if is_ok then 
            print_endline "The flow respects the mass balance contraint"
          else
            print_endline " The flow does not respect the mass balance contraint"
in 
let ()  = print_endline " enter anything to terminate" in 
let _ = read_line () in
!g

