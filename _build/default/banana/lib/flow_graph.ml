module D = Int_dynamic_array
  
  
let  _  = D.empty 
let _  = D.fill 0 0 
type vertex = int 
type edge = vertex *vertex
(*1 array for the : b(i), point(i), rpoint(i),f/r-trace(i),tail,head,cost,capacity*)

type vertex_attributes = {b : int array}
type pointers = { frange : int array ; rrange : int array   ;
                  fpoint : D.t       ; rpoint : D.t        }
type edge_attributes = { map_e_to_num : (edge,int) Hashtbl.t ; 
                    tail : D.t;
                    head : D.t;
                    cost : D.t;
                    capacity : D.t;
                    flow : D.t;
                    edge_type : D.t (*-1 for backard ; 1 for forward*)}

type  t = {mutable r : bool (*residual or not*); v_a : vertex_attributes ; p : pointers ; e_a : edge_attributes  }
type capacity = Capa of int | Infty
type vpath = vertex list
type epath = edge list

exception Unvalid_Capacity_Operation
exception Not_residual_graph of t
exception Uncapacitated_arc of edge


let c_op op c1 c2 = 
  match c1 with
  |Infty -> Infty
  | Capa x -> match c2 with
              | Infty -> Infty
              |Capa y -> Capa (op x y)
    
let (++) = c_op (+)


let ( ** ) c1 c2 = 
    let e = c1,c2 in
    if e  = (Capa 0, Infty) || e = (Infty, Capa 0 )then raise Unvalid_Capacity_Operation 
    else  c_op ( * ) c1 c2

let (>>) c1 c2 = match c1 with  
    | Infty -> ( match c2 with 
                | Infty -> false
                | Capa _ -> true)
    | Capa x -> ( match c2 with
                  | Infty -> false
                  | Capa y -> x>y )

let (--) c1 c2 = 
  if c1 = Infty && c2 = Infty  || c2 >> c1 then raise Unvalid_Capacity_Operation 
  else c_op (-) c1 c2
                
let get_id (g:t) (e:edge) =   Hashtbl.find g.e_a.map_e_to_num e



let create n : t =
  let b = Array.make n 0 in 
  let fpoint = D.create n in
  let rpoint = D.create n in
  let frange = Array.make n (-1)  in
  let rrange = Array.make n (-1) in
  let map = Hashtbl.create n  in
  let tail = D.create n in 
  let head = D.create n in 
  let capacity = D.create n in 
  let cost = D.create n in
  let flow = D.create n in 
  let edge_type = D.create 0 in 

  {r = false ;v_a ={ b} ;p = {fpoint ;rpoint ;frange ;rrange }; 
        e_a = {map_e_to_num = map ; tail; head; capacity ; cost ; flow ;edge_type } }
  
let empty = create 0
let upper_capacity = 999


let n_init g (i,j)= 
D.add g.e_a.tail i ;
  D.add g.e_a.head j ; 
  D.add g.e_a.cost 0 ; 
  D.add g.e_a.capacity (-1);
  D.add g.e_a.flow 0;
  D.add g.p.rpoint (-1);
  D.add g.p.fpoint (-1)

let f_init g e = 
  n_init g e;
  D.add g.e_a.edge_type 1
let b_init g e = 
  n_init g e ; 
  D.add g.e_a.edge_type (-1)
let general_add_edge g init_function (i,j) = 
  let m = D.size g.e_a.tail in
  
  init_function g (i,j);

  let finit_neigh i = 
    g.p.frange.(i) <- m;
    D.set  g.p.fpoint m  (-1)
  in 
  let rinit_neigh i = 
    g.p.rrange.(i) <- m;
    D.set g.p.rpoint m  (-1)
  in
  let fupdate_neigh i = 
    let start = g.p.frange.(i) in
    let next = D.see g.p.fpoint start in
    D.set g.p.fpoint start m;
    D.set  g.p.fpoint m next
  in
  let rupdate_neigh j = 
    let start = g.p.rrange.(j) in
    let next = D.see g.p.rpoint start in
    D.set g.p.rpoint start m;
    D.set  g.p.rpoint m next
  in

  if g.p.frange.(i) = -1 then
    finit_neigh i 
  else 
    (fupdate_neigh i);
  if  g.p.rrange.(j) = -1 then
    rinit_neigh j
  else 
    rupdate_neigh j ;
      
  Hashtbl.add g.e_a.map_e_to_num (i,j) m

let add_fedge g e = general_add_edge g f_init  e
let add_bedge g e = general_add_edge g b_init e 

let add_edge  g e  =  general_add_edge g n_init e


(*modification des pointeurs*)
    
let set_flow g e x = 
  let id  = get_id g e in
  D.set g.e_a.flow id x

let set_cost g e x = 
  let id  = get_id g e in
  D.set g.e_a.cost id x
let set_capacity g e c = 
  let x = match c with  
    |Infty -> -1 
    |Capa x -> x
  in 
  let id  = get_id g e in
  D.set g.e_a.capacity id x

let set_supply g v x =   g.v_a.b.(v)<- x

let size g  =   Array.length g.v_a.b
let nb_edge g = D.size g.e_a.cost 

let veq = (=)
let vcompare = Stdlib.compare
let vhash = Hashtbl.hash
let eeq = (=)
let ecompare = Stdlib.compare
let ehash = Hashtbl.hash


(* *)

let iter_ver f g = 
  let n =  size g in 
  for i = 0  to n-1 do 
    f i
  done
let iter_edg f g  = 
  let m  = D.size  g.e_a.tail in
  for i = 0  to m-1 do
    let e  = D.see g.e_a.tail i , D.see g.e_a.head i in 
    f e
  done

let fold iterator f x0 g = 
  let pre  = ref x0 in 
  let aux x = 
    let temp  =  f !pre x in 
    pre := temp 
  in 
  iterator aux g;
  !pre

let  fold_ver f x0 g = fold iter_ver f x0 g
  
let fold_edge f x0 g  = fold iter_edg f x0 g


let neighbor_ver  range point  what_to_take  v = 
  let ls  = ref []  in 
  let start =  range.(v) in 
  let  k = ref start in
  while !k <> -1 do 
    let v = D.see what_to_take !k in 
    ls := v::!ls;
    
    k:= D.see point !k
  done;
  !ls

let succ_ver  g (v:vertex) :vertex list = 
  neighbor_ver g.p.frange g.p.fpoint   g.e_a.head v
let prede_ver  g (v:vertex):vertex list = 
  neighbor_ver g.p.rrange g.p.rpoint  g.e_a.tail v


let see_name _ v = string_of_int v


let fetch_attribute_e (g:t)  obj (e:edge) = 
  try
    let id  =  get_id g e in
    D.see obj id
  with
    | Not_found -> 0

let see_supply g v  =   g.v_a.b.(v)
let see_flow g e =
  fetch_attribute_e g g.e_a.flow e 
let see_cost g e = fetch_attribute_e g g.e_a.cost e

let see_capacity g e = 
  let x = fetch_attribute_e g g.e_a.capacity e in 
  if x>=0 then 
    Capa x 
  else
    Infty



type residual_edge_type = Forward | Backward

let set_residuality g x =  g.r <- x
let is_residual g = g.r 
let e_type  g e = if not g.r then raise (Not_residual_graph g);
  let id = get_id g e  in 
  let x  = D.see g.e_a.edge_type id
  in
  if x  =  -1 then Backward else Forward



let sing_source_sing_sink g  = 
let n   =  size g  in 
let g' =  create (n+2) in
let total_supply = ref  0  in 

let unseen_v  =  Array.make n true in 
let is_source v  = see_supply g v > 0  in 
let is_sink v  = see_supply g v <0 in 

let connect_to_terminal t v  = 
(*t = 0  ->> connect to mega_source
  t = n+1 ->> connect to mega sink*)
  let b = see_supply g v  in 
  let e = if t <v+1 then (t,v+1) else (v+1,t) in 
  add_edge g' e;
  set_capacity g' e (Capa b);
  (*cost is 0 default*)
  if b > 0 then 
    total_supply :=  !total_supply+ b

in 
let add_source = connect_to_terminal 0  in 
let add_sink  =  connect_to_terminal (n+1) in 
let add_terminal v = 
  if unseen_v.(v) then 
    if is_source v then add_source v ; 
    if is_sink v  then add_sink v 
in 


let process_edge  (i,j) = 
  add_edge g' (i+1,j+1) ; 
  set_capacity g' (i+1,j+1) (see_capacity g (i,j));
  set_flow g' (i+1,j+1) (see_flow g (i,j));
  set_cost g' (i+1,j+1) (see_cost g (i,j));    
  
  add_terminal i ; 
  add_terminal j ;
  unseen_v.(i) <- false;
  unseen_v.(j) <- false
in

iter_edg process_edge g;
set_supply g'  0 !total_supply ; 
set_supply g' (n+1) (- !total_supply ) ;

g'




let uncapacitated_to_capacitated g = 
let aux  e = 
  let c  =  match see_capacity g e with  
              | Infty -> Capa upper_capacity
              | x ->  x 
in set_capacity g e c

in 
iter_edg aux g 

(*let capacitated_to_uncapacitated g  = 
let aux e = match see_capacity g e with
  | Capa x when x = upper_capacity ->  set_capacity g e Infty
  | Capa _ -> ()
  | Infty ->  raise ( Uncapacitated_arc e )
in 
iter_edg aux g 
*)