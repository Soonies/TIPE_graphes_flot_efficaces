module G  = Flow_graph
module Rnd = Random
module P = Plplot.Plot
module H =  Hashtbl
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


let iter_vert_pairs f g  = 
   G.iter_ver ( fun i-> G.iter_ver (f i) g  ) g
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

(*******************brute force*)


let complete_graph_of_graph g  = 
  let f i j  = 
    if i <> j then G.add_edge g (i,j)  
  in
  iter_vert_pairs  f g 
let make_residual  g =
  let ls_vert  =  G.fold_ver (fun x y-> (G.see_label x)::y ) g [] in 
  let rg =  G.create ls_vert in 
  let f i j  = 
    let e =  (i,j) in  
    let u  =  G.see_flow g e in 
    if G.(<<) (Capa u)  (G.see_capacity g e) then (
      G.add_edge rg e ;
      G.set_capacity rg  e (G.(--) (G.see_capacity g e) (G.Capa u)))
  in 
  iter_vert_pairs f g;
  rg 

