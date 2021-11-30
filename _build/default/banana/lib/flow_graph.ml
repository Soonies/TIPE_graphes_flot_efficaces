(**********************************************)
(*Implementation selon la documentation papier*)
(***********************************************)
module H = Hashtbl
module D  =  Dynamic_array
module L = Linked_array

type 'a vertex = V of 'a

type capacity  = Capa of int | Infty

type 'a edge = 'a vertex * 'a vertex

type 'a flow  =  ('a edge -> int) 

type 'a edge_att_rec =  {edge : 'a edge ; cntnt : int array }


type 'a t = {
  v : ('a vertex, int array) H.t ; (* dico attributs des vertex. Attribue array is of type [| i (unique index of the vertex) , b(i)|]*)
  e: ('a edge, int) H.t ; (**dico index des edges dans [edges] *)
  fptr : int L.t ; (* Chaine d'adjacence emmanant*)
  bptr : int L.t ; (* Chaine d'adjacence incident*)
  frange: int array;(*range pour la fchain*)
  brange: int array;(*range pr la bchain*)
  edges : 'a edge_att_rec D.t ; (*edges et attributs*)
  mutable f : 'a flow
  }

  
type 'a vset =  
  |Set of 'a vertex list 
  |FRange of (int*int) * int L.t *('a edge_att_rec D.t)
  |BRange of (int*int) * int L.t * ('a edge_att_rec D.t)

exception Invalid_Capacity_Operation  

exception Calculating_Neg_Capacity

exception Uncapacitated_arc of (string) 

exception Not_residual_graph of string

let (++) a b  =
  match a,b with
  | Capa x, Capa y -> Capa (x+y)
  | Infty , Capa _ | Capa _ , Infty  -> Infty
  | _ -> raise Invalid_Capacity_Operation


let (--) a b  = match a,b with
  | Capa x, Capa y when x>= y -> Capa (x-y)
  | Infty , Capa _  ->  Infty
  | Capa _ , Infty | Capa _, Capa _->  raise Calculating_Neg_Capacity
  | _ -> raise Invalid_Capacity_Operation

let ( ** ) a b  =  match a,b with 
  | Capa x ,  Capa y  ->  Capa (x*y)  
  | Capa x , Infty  | Infty, Capa x   when  x>0 ->  Infty
  | _ -> raise Invalid_Capacity_Operation

let (<<)  a b  =  match a,b with 
| Capa x , Capa y -> x < y
| Capa _ , Infty ->  true
| Infty , Capa _-> false 
| _ -> raise Invalid_Capacity_Operation 

let is_empty g = H.length g.e =0 

let size g  = H.length g.v

let nb_edge  g  = H.length g.e

let see_supply g v = (H.find g.v v).(1)

let index_edge g e  = H.find g.e e 

let index_vertex g v  = (H.find g.v v).(0)

(**[modify_vertex_attribute g v x attribute] *)
let modify_vertex_attribute g v x attribute =
  let attr = H.find g.v v in 
  match attribute  with
  | "indx" ->  attr.(0)<-x
  | "supp" -> attr.(1)<-x
  | _ -> failwith "invalid argument, expected supp/indx as supply ro index"


let set_vertex_index g v x  = modify_vertex_attribute g v x "indx"

let modify_edge_field g n i x = (D.see g.edges n).cntnt.(i)<- x

let see_edge_field g n i = (D.see g.edges n).cntnt.(i)

(**[do_edge_attribute g e x attribute f'] apply the action [f'] on the attribute [attribute] of [e], an edge of [g] and [x], a value. *)
let do_edge_attribute g e x attribute f' = 
  let n = index_edge g e in 
  let f  = f' g n in 
  match attribute with 
  | "tail" | "head"-> failwith "cannot modify tail or head"
  | "capa" -> f 0 x
  | "cost" -> f 1 x
  (*|"flow" ->*)
  |"bindx" -> f 2 x
  | "findx" -> f 3 x
  | _ ->  failwith "Invalid argument, should be tail/cost/capa/bindx/findx"


let modify_edge_attribute g e x attribute = 
  do_edge_attribute g e x attribute  (modify_edge_field) 

let see_edge_attribute g e attribute = 
  do_edge_attribute g e () attribute (fun x y z _-> see_edge_field x y z) 


let see_cost g e  = see_edge_attribute g e "cost"

let see_capacity g e   =
  let c = see_edge_attribute g e "capa" in 
  match c with 
  | x when x>=0 -> Capa x 
  | -1 -> Infty
  | _ -> failwith "uh oh, capacite invalide"


(**controversial*)
let see_flow g e = g.f e

let set_flow_edge g e x = 
  let f' edge  = 
    if edge = e then
     x
    else
    g.f edge
  in 
    g.f <- f'

let set_flow_graph g f  = g.f <- f

let set_cost g e x  = modify_edge_attribute g e x "cost"

let set_capacity g e x  = match x with
  | Capa x  -> modify_edge_attribute g e x "capa"
  | Infty -> modify_edge_attribute g e (-1) "capa"

let set_supply g v x  = modify_vertex_attribute g v x "supp"


let set_bindx g e x  = modify_edge_attribute g e x "bindx"

let set_findx g e x  = modify_edge_attribute g e x "findx"



let create ls = 
  let n =  List.length ls in 
  let g = {
  v = H.create n ; 
  e = H.create n  ;
  fptr = L.create n  ; 
  bptr = L.create n; 
  frange =  Array.make (2*n+1) (0); 
  brange= Array.make (2*n+1) (0); 
  edges =  D.create n; 
  f = (fun _ -> 0)
  } in 
  
  let new_vertex_attribute_array ()  = Array.make 2 0 in 

  let i = ref  1 in
  let f  x = 
    H.add g.v  (V x) (new_vertex_attribute_array ()) ;
    set_vertex_index g (V x) !i;
    set_supply g (V x) 0 ; 
    incr i
  in
  List.iter f ls;
  let _ = L.tail_add g.fptr (-1) in
  let _ = L.tail_add g.fptr (-1) in
  
  g


let see_begining_range range v  = range.(2*(v-1))

let see_end_range range v = range.(2*(v-1)+1 )

let set_begining_range range v x = range.(2*(v-1)+1)<-x

(**[set_end_range range v x] set the end of the range to the value [x] for the vertex with index [v]*)
let set_end_range range v x = range.(2*(v-1)+1 +1)<-x

(** [add_to_chain g i x mode] insterts the index [x] in the chain [mode] for the vertex indexed [i] AND updates the associated range*) 
let  add_to_chain g i x mode = 
  let f range ptr = 
    if see_begining_range range i = 0 then
      let n = L.tail_add ptr x in 
      set_begining_range range i n;
      set_end_range range i n;
      n
    else
      let finish  = see_end_range range i in 
      let n = L.insert_add ptr finish  x in 
      set_end_range range i n;
      n 
    in
  match mode with
  | "fptr" -> f g.frange g.fptr
  | "bptr" -> f g.brange g.bptr
  | _ -> failwith "invalid argument, expected fprt/bptr"


let add_edge g e = 
  let (V a, V b) = e in
  let new_edge_attribute_record  =  {edge = e ; cntnt = Array.make 4 0} in 
  let n_edge  = D.add g.edges new_edge_attribute_record in 
  
  H.add g.e e n_edge;
  let i,j = index_vertex g (V a) , index_vertex g (V b) in 
  let findx  = add_to_chain g j n_edge "fptr" in 
  let bindx  = add_to_chain g i n_edge "bptr" in
  set_findx g e findx ; 
  set_bindx g e bindx


(**[remove_from_chain g   indx i mode] removes the element of index [indx] in the chain [mode] AND updates the associated range for the vertex of index [i] *)
let remove_from_chain g indx i mode  =
  let f  range ptr = 
    if see_begining_range range i = see_end_range range i then (
    set_begining_range range i 0 ; 
    set_end_range range i 0
  ) else
    let prede  =  L.previous ptr indx in 
    set_end_range range i prede;
  L.remove ptr indx;
  in 
  match mode with
  | "fptr" -> f g.frange g.fptr
  | "bptr" -> f g.brange g.bptr
  | _ -> failwith "invalid argument, should be fptr/bptr"


let delete_edge g e  = 
  let (V a, V b) = e in 
  let i,j = index_vertex g (V a) , index_vertex g (V b) in

  let findx  = see_edge_attribute g e "findx" in 
  let bindx  = see_edge_attribute g e "bindx" in
  
  let edge_index  = index_edge g e in 
  H.remove g.e e;
  D.remove g.edges edge_index;
    
  remove_from_chain g findx  i "fptr" ;
  remove_from_chain g bindx  j "bptr" 

let veq = (=)

let eeq = (=)

let vcompare = Stdlib.compare

let vhash = H.hash

let ecompare = Stdlib.compare

let ehash = H.hash

let iter_ver f g = 
  H.iter (fun x _ -> f x) g.v

let iter_edg f g =
  H.iter (fun x _ -> f x) g.e

let fold_ver f g init = 
  H.fold (fun x _ z ->  f x z ) g.v init

let fold_edg f g init = 
  H.fold (fun x _ z ->  f x z ) g.e init


let vset_of_vertex_list ls  = Set ls

let iter_vset f s = 
  
  match s with
  | Set ls  -> List.iter f ls 
  | FRange ((u,v),a,edges) -> (
    let head  _ x =
      let (_,V j)  = (D.see edges x).edge in 
      f (V j)
    in
    L.iterc_range head a u v  )
  | BRange ((u,v),a,edges) -> (
    let tail  _ x =
      let (V i, _)  = (D.see edges x).edge in 
      f (V i)
    in
    L.iterc_range tail a u v  )


let adj_ver  g v mode  = 
  let f range ptr constructor=
    let i  = index_vertex g v  in 
    let a,b  = see_begining_range range i ,  see_end_range range i in 
    constructor ((a,b),ptr,g.edges)
  in match mode with 
  | "prede" -> f g.brange g.bptr (fun (u,v,w) -> BRange (u,v,w))
  | "succ" ->f g.frange g.fptr (fun (u,v,w) -> FRange (u,v,w))
  | _ -> failwith "invalid argument, should be prede or succ"

let prede_ver g v = adj_ver g v "prede"

let succ_ver g v = adj_ver g v "succ"