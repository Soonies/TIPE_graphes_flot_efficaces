(***************************************)
(*Implementation selon la documentation papier*)
(*Type :  foward (P.O.V. = tail , dta = successors) and reverse( POV = HEAD,  dta = predecessors) double array storage +  association dictionary*)
(****************************)
module H  = Hashtbl
module D = Int_dynamic_array

type vertex = int

type capacity = Capa of int | Infty

type edge  =  vertex*vertex

type edge_attribute_record  = {
  flow  :  int ; 
  capacity : capacity ; 
  cost : int 
}

type  t = {
  foward_edges : D.t ;
  reverse_edges : D.t ; 
  attributes_edges : (edge,edge_attribute_record) H.t ;
  vertices_supply : (vertex,int) H.t 
}

type vpath  =  vertex list

type epath = edge list

exception Unvalid_Capacity_Operation

exception Uncapacited_arc of edge

exception Not_residual_graph of t

let (++) c1 c2  = match c1,c2 with 
  | Infty,_ | _,Infty  -> Infty
  | Capa n1, Capa n2 -> Capa (n1 + n2)

(* ...*)
let empty =  {
  foward_edges = D.empty ;
  reverse_edges= D.empty ; 
  attributes_edges =  H.create 1 ;
  vertices_supply =  H.create 1 
}

let create n  = 
  let foward_edges = D.create (2*n+1) in
  let reverse_edges = D.create (2*n+1) in 

  D.set foward_edges 0  (n);
  D.set reverse_edges 0  (n);
  for k=1 to 2*n do
    D.set foward_edges k  (-1);
    D.set reverse_edges k  (-1)
  done;

  let attributes_edges = H.create n in (*n is a reasonnable guess for |E| *)

  let vertices_supply = H.create n in 
  for k= 1 to n-1 do 
    H.add vertices_supply k 0
  done ;
  {foward_edges = foward_edges ;
  reverse_edges = reverse_edges ; 
  attributes_edges = attributes_edges ;
  vertices_supply  = vertices_supply}

(** [set_extremity_chain g extremity mode v  i] change la valeur du debut de chaine pour *)
let set_extremity_chain g extremity mode v  i =
  match (extremity,mode) with
    | "start" , "foward"  -> D.set g.foward_edges (2*v-1) i
    | "start" , "reverse" -> D.set g.reverse_edges (2*v-1) i
    | "end" , "foward"    -> D.set g.foward_edges (2*v) i
    | "end" , "reverse"   -> D.set g.reverse_edges (2*v) i
    | _, _  ->  failwith "Invalid parameter: [extremity] OR [mode]. [extremity] must be one of [start/end] . [mode] must be one of [foward/backward]."


let add_edge g e  = 
  let (u,v) = e in 
  
  (*foward*)


