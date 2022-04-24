module G  = Flow_graph
module Rnd = Random
module P = Plplot.Plot
module H =  Hashtbl

type 'a t  =  { s : 'a G.vertex ; 
               t :  'a G.vertex ; 
               residual :  bool;
               graph : 'a G.t}  

           
type 'a node  = |N of 'a | C of 'a*'a
let square_dim  = 300

let make_canonical g s t r= 
  { s = s ;  t=  t ;  graph  =  g ; residual  = r }
    

(**creates an empty graph off of  n random points on the plane*)
let generate_instance n  = 
  let () = Rnd.self_init () in 
  let f  =  Rnd.int in 
  let s  =  square_dim in

  let rec gen_lst compt accu  = match compt with
    | 0 -> accu 
    | x -> gen_lst (x-1) ((f s , f s):: accu)
  in
  G.create (gen_lst n [])

(**iter all vertex pairs {i,j}, i<>j (if (i,j) already see, wont explore (j,i))*)
let iter_vert_pairs f g  = 
  let deja_vu  =  H.create (G.size g) in
  let aux i j  =  
    if i<> j && not ((H.mem deja_vu (i,j)) || (H.mem deja_vu (j,i))) then
      f i j;
      H.add deja_vu (i,j) ()
    in 
   G.iter_ver ( fun i-> G.iter_ver (aux i) g  ) g

(**completes the graph*)
let complete_graph_of_graph g  = 
  let f i j  = 
    G.add_edge g (i,j) ;
    G.add_edge g (j,i) 
  in
  iter_vert_pairs  f g  


let pow_crasse x n  =x |> float_of_int |> fun x ->  x ** n |> int_of_float  
 let cout (i,j)  = 
    let (i1,i2) ,  (j1,j2)   = G.see_label i , G.see_label j in 

      (abs (i1-j1) * abs (i1-j1) * abs (i1-j1)* abs (i1-j1)  + abs (i2-j2)  * abs (i2-j2) * abs (i2-j2) * abs (i2-j2)  ) 
    |> float_of_int |>( fun x ->  (x ** (1./.4.))*. 100000.)|>  int_of_float (*  *)  
let couti (i,j)  = 
    let (i1,i2) ,  (j1,j2)   = G.see_label i , G.see_label j in 

      abs ( abs (i1-j1) * abs (i1-j1)+ abs (i2-j2)  * abs (i2-j2))  (* 
    |> float_of_int |>( fun x ->  (x ** (1./.4.))*. 100000.)|>  int_of_float *)  

(**fill the edges cost*)
let fill_cost g  = 

  let f (i,j)  = 
   let d  = cout (i,j)  in 
   G.set_cost g (i,j) d  
  in
   G.iter_edg f g 

(**transformation to no parallel edged graph | input : grpah | output:  corresponding graph with 'a node vertex type ok?! apparemment*)
let to_no_parallel_edges g  = 
  let n  = G.size g in 
  (*parse edge1 to compute all vertices*)
  let ls_vert =  ref [] in 
  let deja_vu_edge = H.create  n in
  let deja_vu_vert = H.create n in  
 
  let edge_stack = Stack.create () in
  let f  =  G.see_label in 

  let parse_edge1 (i,j)  = 
    let parse_vert i  = 
      if not (H.mem deja_vu_vert i) then(
        ls_vert := N (f i) :: !ls_vert;
        H.add deja_vu_vert i ())
    in 
    
    if G.edge_in_graph g (j,i) && H.mem deja_vu_edge (j,i) then (
      ls_vert := C (f i,f j) ::!ls_vert;
      Stack.push (N (f i) ,C (f i,f j)) edge_stack;
      Stack.push (C (f i,f j),N (f j)) edge_stack;
      H.add deja_vu_edge (i,j) () )
    else(
    parse_vert i;
    parse_vert j;
    Stack.push (N (f i), N (f j)) edge_stack;
    H.add deja_vu_edge (i,j) ();
    )
  in
  G.iter_edg parse_edge1 g ; 
  let g' = G.create !ls_vert in 
  (*parse vertices*)
  let parse_vertex v  =  match G.see_label v with
    | N x -> G.set_supply g' v (G.fsee_supply g x)
    | C (_,_) -> () (*supply is 0 by default*)

  in 
  G.iter_ver parse_vertex g';

  (*parse edge 2*)
  let parse_aux a b capa cost  = 
    G.fadd_edge g' a b ;
    if G.is_infty_capa g' capa then
      G.fset_infty_capa g' a b 
    else
      G.fset_capacity g' a b capa
    ;
    if G.is_pos_infty_cost g' cost then
      G.fset_cost g' a b (-1)
    else if G.is_neg_infty_cost g' cost then
      G.fset_cost g' a b (-2)
    else
      G.fset_cost g' a b cost
in
  let parse_edge2 e  =  match e with
    | N i, N j -> parse_aux (N i) (N j ) (G.fsee_capacity g i j) (G.fsee_cost g i j);
        
    | N k,  C (i,j) when k= i -> parse_aux (N k) (C (i,j)) (G.fsee_capacity g i j) (G.fsee_cost g i j);
       
    
    | N _ , C(_,_) -> failwith "Tried to link Normal edge with non related 'reverse vertex' / to_no_parallel_edges ->  parse_edges "
    | C(i,j) ,  N k  when j = k -> parse_aux   (C(i,j))  (N k) (-1) 0; (*capa infinie*)
    |  C(_,_) ,  N _ -> failwith "Tried to link  'reverse vertex'  with non related Normal edge / to_no_parallel_edges ->  parse_edges "
    | _ -> failwith "Tried to link 2 reverse vertices /  to_no_parallel_edges ->  parse_edges "
  in
  Stack.iter parse_edge2 edge_stack ;
  g'

let from_no_parallel_edges g'  = 
  let ls  =  G.fold_ver (fun x ls-> match G.see_label x with |   N i -> i::ls | C (_,_) -> ls) g' [] in
  let g  = G.create ls in 

  let parse_vertex v  = match G.see_label v with
    | N x -> G.fset_supply g x (G.see_supply g' v) 
    | C(_,_) ->  ()
  in
  G.iter_ver parse_vertex g'; 

  let parse_aux a b capa cost flow = 
    G.fadd_edge g a b ;
    if G.is_infty_capa g capa then
      G.fset_infty_capa g a b 
    else 
      G.fset_capacity g a b capa;
    G.fset_cost g a b cost;
    G.fset_flow g a b flow
  in
  let f (i',j') =
    let i,j = G.see_label i' ,  G.see_label j'  in match i,j with
    | N k , C(a,_) when k  <> a -> failwith "Link between Normal vetex and unrelated reverse vertex/ from_no_parallel_edges "
    | N a, N b | N _ , C(a,b) -> parse_aux a b (G.see_capacity g' (i',j') ) (G.see_cost g' (i',j')) (G.see_flow g' (i',j'))
    | C(_,b) , N k when b<> k -> failwith "Link between  unrelated reverse vertex abd Normal vetex / from_no_parallel_edges " 
    | C(_,_) , N _ -> () 
    | C (_,_) , C (_,_) -> failwith "Link between to reverse vertices /  from_no_parallel_edges"
  in 
  G.iter_edg f g';
  g

(**copies the graph  edges and attributes + adds the vertices in ls (doesnt copy the supplies) |input ~graph ~vertex list to be added | Output : new graph *)
let copy_graph_including_vertices g ls exclude = 
  
  let excluded_v   =  H.mem exclude  in
  let excluded_e  (i,j) =  excluded_v i || excluded_v j  in
  
  let ls_vert =  ls @ ( G.fold_ver (fun x ls -> if excluded_v x then ls else ( (G.see_label x )::ls )) g [] ) in 
  let g'  =  G.create ls_vert  in 
  (* add the edges*)
  G.iter_edg (fun e -> if not (excluded_e e) then G.add_edge g' e ) g;

  (*parse the edges*)
  let parse_cost  e = 
    if excluded_e e then () else ( 
    let c  = G.see_cost g e in 
    if G.is_pos_infty_cost g c then (
      G.set_pos_infty_cost g' e
    ) else if G.is_neg_infty_cost g c then (
      G.set_neg_infty_cost g' e
    ) else (
      G.set_cost g' e c
    ))
  in
  let parse_capa  e = 
  if excluded_e e then () else (
  let u  =  G.see_capacity g e in 
    if G.is_infty_capa g u then(
      G.set_infty_capa g e 
    ) else (
      G.set_capacity g e u
    ))
  in 
  G.iter_edg parse_cost g ;
  G.iter_edg parse_capa g;
  G.iter_edg (fun x -> if excluded_e x then () else  G.set_flow g' x (G.see_flow g  x) ) g;
  
  g'

(** reduces a multi source network to sing sourcce sing sink: ok! *)
let to_s_source_s_sink g = 
 
  let s  =  G.make_vertex (N (-1,-1)) in 
  let t  =  G.make_vertex (N (square_dim +1,  square_dim +1)) in 
  
  let g'   = copy_graph_including_vertices g [N(-1,-1);N (square_dim + 1  ,  square_dim +1) ] (H.create 0) in
  let f i =  
    
    let b  = G.see_supply g i in
    
    if b > 0 then  ( 
      G.add_edge g' (s, i) ; 
      G.set_capacity g' (s,i) b)
    else if b<0 then  (
      G.add_edge g' (i, t) ; 
      G.set_capacity g' (i,t) (-b))
  in
  G.iter_ver f g;

  { s = s ; 
    t = t;
    residual  =  false ; 
    graph = g'} 


let from_s_source_s_sink g  =  
  let s  =  g.s in 
  let t = g.t in  

  let exclude  =  H.create 2  in 
  H.add exclude s () ; 
  H.add exclude t () ;

  let g' = copy_graph_including_vertices g.graph [] exclude in  
  let parse_source_sink (i,j) = match ( i ,   j) with
    | (u,v) when v = t -> G.set_supply g' u (-(G.see_capacity g.graph (u,t)))
    | (u,v) when u = s -> G.set_supply g' v (G.see_capacity g.graph (s,v))
    | (_,_) -> ()
  
in G.iter_edg parse_source_sink g.graph;
  g'




(**breadth first traversal of vertx reacheabl from source,  input:  graph ~ source | output: dict with  {vertx : predecessor of vertex / set to itself if source *)
let bfs f g s  = 
  (*bfs can not pretend to find the distance betwen two vertices in a weighted graph*)
  let n = G.size g  in
  let dico_prede = Hashtbl.create n  in 
  let deja_vu  =  H.create n  in 
   
  H.add dico_prede s s;
  H.add deja_vu s (); 

  let queue =  Queue.create ()  in 
  Queue.push s  queue;  
  
  while not (Queue.is_empty queue) do 
    let u  =  Queue.pop queue  in 
    f u ;
    let voisins  =  G.succ_ver g u  in 
    let traite_v v = 
      if not (H.mem deja_vu v) then( 
        H.add dico_prede v u;
        Queue.push v queue;
        H.add deja_vu v ())
    in
    G.iter_vset traite_v voisins;
    
  done;
    dico_prede
  
(** ok retrace  s-t  path with a bfs, input : graph ~ source ~ goal | output :  Some vertx list representing  s-t path /  None if none *) 
let path_bfs g s t  =  
  
  let dico_prede = bfs (fun _ -> ()) g s in 
  let rec retrace v ls = 
    let prede = H.find dico_prede v in 
    if prede = v then 
      ls
    else
      retrace prede (prede::ls)
  in
  try 
    Some (retrace t [t])
  with
    | Not_found -> None

(**ok transform vertex path to edge path (reversed order) |input  =    goal ~ vertex path  | output = edge path as a list (reversed order)  *)
let make_edge_path t p = 
      let rec aux p ls = match p with
        | [x;y] when y  =  t -> (x,t) :: ls
        | [_;_] -> failwith "s-t path that doesnt end with t / make_edge_path" 
        | [_] -> failwith "lone vertex /  make_edge_path"
        | [] -> failwith "unexpected behaviour / make_edge_path"
        | i::j:: tl -> aux (j::tl) ((i,j)::ls) 
in aux p []

(**shortest path and prede of all pair shortest path, input  = graph | output =  (Some distance / infty) matrix * (Some predecessor / Nil) matrix * dico vertex ->  unique vertx id*)
let floyd_warshall g  =  
  let n  =  (G.size g) in
  
  let k  = ref  0  in 
  let v2id = H.create n in 
  let id2v = H.create n in 
  let f i = 
    H.add v2id i !k;
    H.add id2v !k i ;
    incr k
  in G.iter_ver f g  ; 
  let toid (i,j)  =  H.find v2id i, H.find v2id j in 

  let d  =  Array.make_matrix n n (None) in
  let pi  = Array.make_matrix n n (None) in 
  let f e = 
    let i',j'  = toid e in   
    d.(i').(j') <- Some (G.see_cost g e);
    pi.(i').(j') <- Some (i')
 
  in  G.iter_edg f g;
  G.iter_ver (fun i -> let i' = H.find v2id i in d.(i').(i') <- Some 0 ; ) g;

  for l = 0 to n-1 do
    for i  = 0 to n-1 do 
      for j  =  0 to n-1 do 
        print_int l ; print_int i ; print_int j ;
        let a,b, c  =  d.(i).(j) , d.(i).(l) , d.(l).(j) in 
          match a,b,c with
          |Some x, Some y, Some z ->  
            if x>  y+ z then(d.(i).(j) <- Some (y+z) ;
              pi.(i).(j) <- pi.(l).(j))
          | _ , _, None| _, None, _ -> () 
          |None, Some y, Some z  -> 
            d.(i).(j) <- Some (y+z) ;
            pi.(i).(j) <- pi.(l).(j)
  done; done; done;
  d,pi,id2v

let make_residual  g'  =
  (*NO PARALLEL EDGES PLEASE*)
  let g  = g'.graph in 
  let ls_vert  =  G.fold_ver (fun x y-> (G.see_label x)::y ) g [] in 

  let rg =  G.create ls_vert in 
  
  let parse_edge e  = 
    let (i,j) = e in  
    let c  =  G.see_cost g e in
    let x  =  G.see_flow g e in 
    let u  =  G.see_capacity g e in 
    if x <  (G.see_capacity g e) then (
      G.add_edge rg e ;

      if  G.is_infty_capa g u then (
        G.set_infty_capa  rg  e 
      ) else(
        G.set_capacity rg  e ( (G.see_capacity g e) - x)
       );

      G.set_cost rg e c)
    ; if x> 0  then (
        G.add_edge rg (j,i);
        G.set_capacity rg (j,i) x ;
        G.set_cost rg (j,i) (-c) ; 
    );
  in 
  G.iter_edg parse_edge g;
  {s = g'.s ; 
  t = g'.t;
  residual = true ; 
  graph = rg}

(**transfers the flow from e residual graph to original one | input : graph~resi greph |output  = None but original graph modified with corresponding flow in resi grph*)
let update_dual_with_residual g' rg'  =
  if not rg'.residual then failwith "Not a residual graph /residual_to_dual" 
  else if g'.residual then failwith "Residual original graph / residual_to_dual" else (
  
  let g  = g'.graph in
  let rg =  rg'.graph in 
  let get_flow (i,j)  = 
    if G.edge_in_graph rg (j,i) then
      let xij  =  G.see_capacity rg (j,i) in  
      G.set_flow g (i,j) xij
  in
  G.iter_edg get_flow g
  )

(**finds min capacity along edge list path *)
let find_min_cap g' p  =
  let g = g'.graph in 
  let mini  =  ref  (p |>List.hd |> (G.see_capacity g  )) in 
  List.iter (fun x -> if (G.see_capacity g x )< !mini then mini :=  G.see_capacity g x) p;
  !mini

(**input :  residual graph  | output : None /  modifies the ressidual as if normal graph +x flow along path p *)
let push_flow_residual rg'  p  x = 
  let rg  = rg'.graph in   
  if x <0 then failwith "Tried to push Negative flow / push_flow_residual" 
  else if x = 0 then failwith "Tried to push 0 flow -> inefficient / push_flow_residual" ;

  let f (i,j) = 
    let cost = G.see_cost rg (i,j) in (*saved fot later in case we have to add the reverse edge*)
    let uij = G.see_capacity rg (i,j) in 
    
    ( if not ( G.is_infty_capa rg uij) then (
        (* no need to change capa, stays unchanged after pushing flow*)
        if uij -  x > 0 then(
              uij - x |> G.set_capacity rg (i,j) 
            )else (
              G.delete_edge rg (i,j));
    )
    );
    
    if G.edge_in_graph rg (j,i) then ( 
      let uji  =  G.see_capacity rg (j,i) in 

      if not (G.is_infty_capa rg uji) then (
       G.set_capacity rg (j,i ) (uji+x))
    )else(
       G.add_edge rg (j,i);
       G.set_cost rg (j,i) (-cost);
       G.set_capacity rg (j,i) x;
     )

  in
  List.iter f p


(**modifies a residu to have max flow , input: residual graph  ~ aughmenting path finding functio-> output: None*)
let floyd_fulkerson rg find_path = 
    let t = rg.t in  
    let opt_path  = ref (find_path rg ) in 
    while !opt_path <> None do
      
      let p' = match !opt_path with | Some x -> x | None -> failwith "None path /  floyd_fulkerson" in
      let p  =  make_edge_path  t p' in 
      let min_cap  =  find_min_cap rg p in 
      push_flow_residual rg p min_cap ; 
      opt_path := find_path rg; 
        
    done
  
(** floyd fulkerson with bfs for shortest path | input : ~redial graph | output  =  none but max flowed*)
let edmonds_karp rg = floyd_fulkerson rg  (fun g' -> let g = g'.graph in  path_bfs g (g'.s) (g'.t ))

(** retrace predecessor of given vertex in path until loops back at he start | input  : vertex ~ dict of prede ~ g to have a majoration of number of edges | output : vertex path as  list*)
let retrace_cycle v prede g =
  if H.length prede  <=1  then failwith "Dict with predecessors has less than 1 vertex passed ( could be empty or just 1) / retrace_path";
  
  
  let ls  =  ref [] in
  let current  = ref v in  
  let count  =  ref 0  in 
  let m  = G.nb_edge g in 
  try
    (*run 1 *)
    let deja_vu = H.create m in 
    while not (H.mem deja_vu !current) do
      if !count > m+1 then failwith "Infinite loop, dict corrupted / retrace_cycle";
      H.add deja_vu !current ();
      current := H.find prede !current ;
      incr count 
    
    done;
    let w =  !current in 
    ls := [w];
    count := 0;
    (*run 2*)
    while H.find prede !current <> w do 
      if !count > m+1 then failwith "Infinite loop, dict corrupted / retrace_cycle";
      current := H.find prede !current ;
      ls :=  !current::!ls;
      incr count
    done;
    w::!ls
  with
  | Not_found -> failwith " Failed to find a predecessor of a vertex in Dict = Dict corrupted / retrace_cycle " 


(** runs bellman-ford (label correcting alg) to detect neg cycle| Input :  a graph  | output : vertex Path as list representing cycle u0.. v1.. ... v.. u0*)
exception Break
let bellmann_ford_neg_cycle rg' = (*j'exclue les C ? au fait? *)
  let rg =  rg'.graph in
  let g  = copy_graph_including_vertices rg [N(-2,-2)] (H.create 0) in  (*copy of graph+ slack edge*)
  let s' = G.make_vertex (N(-2,-2) )in  (*slack vertex*)

  let n  = G.size g in 
  let distance_dict =  H.create n  in 
  let prede_dict  =  H.create n  in
  H.add distance_dict s' 0 ;
  H.add prede_dict s' s';

  G.iter_ver (fun v -> 
    if (match G.see_label v with | C (_,_) -> false | N _ -> true) then 
    G.add_edge g (s',v) ) rg;
  
  let relax_edge (u,v)  = 
      if (H.mem distance_dict u) then (

        let du  = H.find distance_dict u  in 
        if (H.mem distance_dict v) then (
          let dv  =  H.find distance_dict v  in 
          if dv > du +  G.see_cost g (u,v) then(
            H.replace distance_dict v (du + G.see_cost g (u,v)) ; 
            H.replace prede_dict v u;
            
          );
        ) else (
          H.replace distance_dict v (du + G.see_cost g (u,v) ); 
          H.replace prede_dict v u;
          
        );
      )
    in 

  for _  = 1 to n-1 do 
    G.iter_edg relax_edge g ; 
   done;

  (* checks for the presence of neg cycle*)

  let start_cycle  = ref s'  in  (*just to fill in, in the case of the presence of a cycle, this value will be replaced. Otherwise, stays unused*)
  let cycle  =  ref None  in 

  let test_relax_edge (u,v)  = (* if able to relax an edge -> neg cycle *)
      if (H.mem distance_dict u) then (
        let du  = H.find distance_dict u  in 
        if (H.mem distance_dict v) then (
          let dv  =  H.find distance_dict v  in 
          if dv > du +  G.see_cost g (u,v) then(
            start_cycle := u  ;  
            raise Break
          );
        ) else (
          failwith "All vertices should have been discovered at this stage. Review the code. / bellman_ford_neg_cycle ->  test_relax_edge"
        );
      )
    in

  (try
    G.iter_edg test_relax_edge rg ; 
  with
    | Break -> cycle :=  Some (retrace_cycle (!start_cycle) prede_dict g  );
  );
  !cycle



let cycle_canceling rg'  =

  let neg_cycle  = ref  (bellmann_ford_neg_cycle rg' ) in 

  while !neg_cycle <> None do 
     let w  =  match !neg_cycle with
      | Some ls-> make_edge_path (List.hd ls) ls 
      | None -> failwith "Neg_cycle is none , unexepected / cycle_canceling"
  in 
    
    let min_capa  = find_min_cap rg' w  in 
    push_flow_residual rg' w min_capa ;
    neg_cycle := bellmann_ford_neg_cycle rg';
    done
 

let min_cost g = 

  complete_graph_of_graph g;
  fill_cost g;


  let g'  =  to_no_parallel_edges g |> to_s_source_s_sink in
  let rg  =  make_residual g' in
  

  edmonds_karp rg;
 cycle_canceling rg; 
 update_dual_with_residual g' rg ; 
  let g'' =  g' |> from_s_source_s_sink |> from_no_parallel_edges in 
  g''


let write_in_log g  = 
  let f v = 
  let x,y = G.see_label v in 
 " ( " ^ string_of_int x  ^ " , " ^ string_of_int y  ^ " ) " ^string_of_int (G.see_supply g v) ^"\n"
  in 

  let channel = open_out "enregistrement.txt" in 

  let ff v stri= f v ^ stri in 
  let sss = G.fold_ver ff  g "" in 
  Printf.fprintf channel "%s\n" sss;
  close_out channel


let total n  flow_value = (*que de nombres pairs svp*)
  Rnd.self_init ();
  let g  =  generate_instance n  in 
  let k  = ref 1  in
  let total  =  ref 0 in
  let parse_supply v  = 
    let b = (*if !k mod 2  = 0  then 1 else -1 in*)  let r = (Rnd.int (flow_value/n +1) )  - (flow_value/(2*n)) (*-*) in if r=0 then 1 else r  in 
    print_int b ; 
    if !k  =  n then(
      if !total = 0 then
        G.set_supply g v 0
      else(
      G.set_supply g v (- !total) );
    
      incr k
    )else(
      total := !total + b ; 
      G.set_supply g v b;
incr k
    ) 
  in 
  
    G.iter_ver parse_supply g ;
    write_in_log g;
  min_cost g
  


let total2 n  flow_value = (*que de nombres pairs svp*)
  Rnd.self_init ();
  let g  =  generate_instance n  in 
  let k  = ref 1  in
  let total  =  ref 0 in
  let neg  = 2 in 
  let bs  =  Stack.create () in 
  let t  =  ref (G.get_1_vert g )in 
  let t'  = ref (G.get_1_vert g  )in 

  for i = 1  to n do
     if i<= n- neg then (
      let r = max (Rnd.int (flow_value/n +1) ) 1  in 
      Stack.push r bs;
      incr k;
      total := r + !total
     ) else (
        let  r =  (if !k  = n then - !total else  min (-Rnd.int (flow_value/n +1) ) (-1))  
        in Stack.push r bs;
        incr k;
        total := r + !total
      )

  done ; 
  k:= 0 ; 
    
  G.iter_ver (fun v -> 
    let b  =  Stack.pop bs in 
    if!k = 0 then 
      t' := v
  else if !k =  1 then
      t := v ;
 
     G.set_supply g v b;
     incr k) g;
  

  let mini  =  ref (-1)  in  
  let mini_vert = ref (G.get_1_vert g) in 

  
 let f g v = 
  let x,y = G.see_label v in 
 print_string " ( "; print_int x; print_string " , "; print_int y  ; print_string " )   b  = " ; print_int (G.see_supply g v) ; print_newline () 
  in  
  G.iter_ver ( fun v  -> 
    if v <> !t' && v <> !t then 
      let cc  = cout ( v ,  !t)  - cout (v,!t' ) in 
      if cc < !mini then(
        mini:= cc ; 
        mini_vert := v;
        print_int cc ; print_string "  " ; f g v ; print_newline () ; )
         ) g ; 
  
 
  f g !mini_vert ; 
  min_cost g
  