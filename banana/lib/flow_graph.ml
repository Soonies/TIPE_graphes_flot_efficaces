(**********************************************)
(*Implementation selon la documentation papier*)
(***********************************************)
module H = Hashtbl
module D  =  Dynamic_array
module L = Linked_array

type 'a vertex = V of 'a


(*private type*)

type cost  =  |PInfty | NInfty | C of float

type 'a edge = 'a vertex * 'a vertex

type 'a edge_att_rec =  {edge : 'a edge ; cntnt : cost ref * int array }


type 'a t = { mutable uU  :  int ;
              mutable cC : float  ; 
  v_representant : 'a vertex ; 
  v : ('a vertex, int array) H.t ; (* dico attributs des vertex. Attribue array is of type [| i (unique index of the vertex) , b(i)|]*)
  e: ('a edge, int) H.t ; (*dico index des edges dans [edges] *)
  edges : 'a edge_att_rec D.t ; (*edges et attributs*)
  fptr : int L.t ; (* Chaine d'adjacence emmanant*)
  bptr : int L.t ; (* Chaine d'adjacence incident*)
  frange: int array;(*range pour la fchain*)
  brange: int array;(*range pr la bchain*)
  }



type 'a vset =  
  |Set of 'a vertex list 
  |FRange of (int*int) * int L.t *('a edge_att_rec D.t)
  |BRange of (int*int) * int L.t * ('a edge_att_rec D.t)


exception Uncapacitated_arc of (string) 

exception Not_residual_graph of string


let is_empty g = H.length g.e =0 

let infty_capa g  =  g.uU 
let pos_infty_cost g =  g.cC

let neg_infty_cost g =  -. g.cC

let is_infty_capa  g n  =  n = g.uU
let is_pos_infty_cost g n  =  n =  g.cC
let is_neg_infty_cost g n  =  n  =  -. g.cC

let size g  = H.length g.v

let nb_edge  g  = H.length g.e

let see_label (V i )  = i  

let see_supply g v = (H.find g.v v).(1)

(**mettre a jour la capa maxi g.uU:  test si deja inferieure , si non fait le changement*)
let update_uU g x  = if x >= g.uU then
    g.uU <- x + 1


(**mettre a jour le cout maxi g.cC:  test si deja inferieur , si non fait le changement*)
let update_cC g x  = if x >= g.cC then
    g.cC <- x +. 1.

(**trouve index de l'edge e dans g.edges (table d'attributs etc)*)
let index_edge g e  = H.find g.e e 

(**trouve index de l'edge e dans g.edges*)
let index_vertex g v  = (H.find g.v v).(0)


(**[do_edge_attribute g e x attribute f'] apply the action [f'] on the attribute [attribute] of [e], an edge of [g] and [x], a value. *)
let do_edge_int_attribute g e x attribute f' = 
  let n = index_edge g e in 
  let f  = f' g n in 
  match attribute with 
  | "tail" | "head"-> failwith "cannot modify tail or head"
  | "capa" -> f 0 x
  | "cost" -> failwith "Cost has a special type / do_edge_int_attribute"
  | "bindx" -> f 1 x
  | "findx" -> f 2 x
  | "flow" ->  f 3 x
  | _ ->  failwith "Invalid argument, should be tail/cost/capa/bindx/findx/flow"

let modify_edge_int_attribute g e x attribute = 
  let modify_edge_field g n i x = (((D.see g.edges n).cntnt) |> snd ).(i)<- x in 
  do_edge_int_attribute g e x attribute  (modify_edge_field) 

let see_edge_int_attribute g e attribute = 
  let see_edge_field g n i = (((D.see g.edges n).cntnt) |> snd ).(i) in 
  do_edge_int_attribute g e () attribute (fun x y z _-> see_edge_field x y z) 


let do_edge_cost_attribute g e x  f' = 
  let n = index_edge g e in 
  let f  = f' g n in 
  f x
  
let modify_edge_cost_attribute g e x = 
  let modify_edge_field g n x = (((D.see g.edges n).cntnt) |> fst ) := x in 
  do_edge_cost_attribute g e x (modify_edge_field) 

let see_edge_cost_attribute g e  = 
  let see_edge_field g n = !(((D.see g.edges n).cntnt) |> fst ) in 
  do_edge_cost_attribute g e ()  (fun g n _ -> see_edge_field g n ) 


let set_pos_infty_cost g e  = 
  modify_edge_cost_attribute g e (PInfty)
let set_neg_infty_cost g e  = 
  modify_edge_cost_attribute g e (NInfty)

let set_infty_capa g e  = 
  modify_edge_int_attribute g e (-1) "capa"

let edge_in_graph g e  = H.mem g.e e

let vertex_in_graph g v = H.mem g.v v
  
let see_flow g e = 
  if not (edge_in_graph g e) then failwith "Tried see flow but Edge doesnt exist / see_flow" else
     see_edge_int_attribute g  e "flow"

let see_cost g e  = 
  if edge_in_graph g e then( 
    let costt =   see_edge_cost_attribute g e in 
    match  costt with
    | PInfty -> g.cC
    | NInfty -> -. g.cC
    | C cost -> cost 
  )
  else  failwith "Tried see cost but edge does not exist / see_cost"


let see_capacity g e   =
  if edge_in_graph g e then 
    let capa  =  see_edge_int_attribute g e "capa"  in 
      if capa =  -1 (*infinie*) then
        g.uU
      else 
        capa
  else failwith " this edge does no exist / see_capacity"


let get_1_vert g = g.v_representant

let get_1_edge g = 
  if is_empty g then failwith "empty graph :  no edges /get_1_edge" else
    (D.get_1_elmnt g.edges).edge



let  get_1_of_blank  key g = 
  if is_empty g then failwith "empty graph :  no edges /get_1_of_blank" else
  match key with
  | "capa" -> let e = get_1_edge g in 
                see_capacity g e
  | "flow" ->  let e = get_1_edge g in 
                                  see_flow g e 
  | "supply"-> let v  =  get_1_vert g in see_supply g v 
  | _ -> failwith "pa bon key"


let get_1_capa g  =  get_1_of_blank "capa" g

let get_1_flow g  = get_1_of_blank "flow" g 

let get_1_cost g  = let e = get_1_edge g in see_cost g e

let get_1_supply  g = get_1_of_blank "supply" g


let set_flow g e x = 
  if not (edge_in_graph g e ) then failwith " Tried to set edge flow but edge does not exist/ set_flow" else(
  (*updates max capa*)
  let u_e = see_capacity g e in 
  if (is_infty_capa g u_e) && x > u_e then 
    g.uU <- x+g.uU;   
  modify_edge_int_attribute g e x "flow")

let set_cost g e x  = 
  if not (edge_in_graph g e ) then failwith " Tried to set edge cost but edge does not exist/ set_cost" ;
  modify_edge_cost_attribute g e (C x)  ; 
  update_cC g x

let set_capacity g e x  = 
  if not (edge_in_graph g e ) then failwith " Tried to set edge caoacity but edge does not exist/ set_capacity" else(
  modify_edge_int_attribute g e x "capa"; 
  update_uU g x)

let set_bindx g e x  = 
  if edge_in_graph g e then
    modify_edge_int_attribute g e x "bindx"
  else 
    failwith  " Tried to set_bindx  but edge does not exist/set_bindx"

let set_findx g e x  =
  if edge_in_graph g e then
     modify_edge_int_attribute g e x "findx"
  else 
    failwith  " Tried to set_findx  but edge does not exist/set_findx"




(**[modify_vertex_attribute g v x attribute= indx ou supp] modifies the attribute attribute of v with x in g*)
let modify_vertex_attribute g v x attribute =
  let attr =  H.find g.v v in 
  match attribute  with
  | "indx" ->  attr.(0)<-x
  | "supp" -> attr.(1)<-x
  | _ -> failwith "invalid argument, expected supp/indx as supply ro index"


let set_supply g v x  = 
if not (vertex_in_graph g v ) then failwith " Tried to set vertex supply but vertex does not exist/set_supply" else(
  modify_vertex_attribute g v x "supp";
  update_uU g x)

let create ls = 
  let n =  List.length ls in 
  let g = {
    uU = 1 ; 
    cC = 1. ;
    v_representant = V (List.hd ls);
    v = H.create n ; 
    e = H.create n  ;
    fptr = L.create n  ; 
    bptr = L.create n; 
    frange =  Array.make (2*n) (-1); 
    brange= Array.make (2*n) (-1); 
    edges =  D.create n; 
  } in 
  
  let new_vertex_attribute_array ()  = Array.make 2 0 in 

  let i = ref  0 in
  let f  x = 
    H.add g.v  (V x) (new_vertex_attribute_array ()) ;
    
  let set_vertex_index g v x  = modify_vertex_attribute g v x "indx" in 
    set_vertex_index g (V x) !i;
    set_supply g (V x) 0 ; 
    incr i

  in
  List.iter f ls;
  g


let  make_vertex v = V v 

let make_edge i j :'a edge= i , j

let see_range_begining range v  = range.(2*v) (* les range commencent des l'indice i = 0 }*) (******************************************************************)

let see_range_end range v = range.(2*v+1 )

let set_range_begining range v x = range.(2*v)<-x

(**[set_range_end range v x] set the end of the range to the value [x] for the vertex with index [v]*)
let set_range_end range v x = range.(2*v+1)<-x

(** adds the vertex with id x to the corresponding neighbour chain relative to the vertex with v_index*) 
let  add_to_chain g mode v_index x  = 
  let f range chain = 
    if see_range_begining range v_index = -1 then (*no succ or prede*)
      
      let n = L.tail_add chain x in 
      set_range_begining range v_index n;
      set_range_end range v_index n;
      n
    else
      let finish  = see_range_end range v_index in 
      let n = L.insert_add chain finish  x in 
      set_range_end range v_index n;
      n 
    in
  match mode with
  | "fptr" -> f g.frange g.fptr
  | "bptr" -> f g.brange g.bptr
  | _ -> failwith "invalid argument, expected fprt/bptr"


let add_edge g e = 
  let (V a, V b) = e in
  let attributes = Array.make 4 0   in 
  attributes.(0) <- (-1) ; (*capa infie par defaut*)

  let new_edge_attribute_record  =  {edge = e ; cntnt = ref (C 0. ) ,attributes} in 
  let n_edge  = D.add g.edges new_edge_attribute_record in (* indice de l'edge dans g.edges*)
  
  H.add g.e e n_edge; (*enregistrement dans le registre*)
  let i,j = index_vertex g (V a) , index_vertex g (V b) in 
  let findx  = add_to_chain g "fptr" i n_edge  in 
  let bindx  = add_to_chain g "bptr" j n_edge  in
  set_findx g e findx ; 
  set_bindx g e bindx


let add_edge_list g ls  = List.iter (fun x -> add_edge g x ) ls

(**[remove_from_chain g   indx i mode] removes the element of index [indx] in the chain [mode] AND updates the associated range for the vertex of index [i] *)
let remove_from_chain g e_indx i mode  =
  let f  range chain = 
    let begining = see_range_begining range i in 
    let finish =  see_range_end range i in 

    if begining = finish then (
    set_range_begining range i (-1) ; (*le range devient vide (on a enleve le dernier edge)*)
    set_range_end range i (-1)
  ) else(
    if begining = e_indx then (
      let nexteuh  =  match L.next chain e_indx with
        | Nil ->  failwith "fptr chain corrupted, reached the end unexpectedly (investigate add_edge, and everything retaed to fptr maintainance / remove_from_chain"
        | P x  -> x
    in 
      set_range_begining range i (nexteuh) );
    
    if finish  =  e_indx then (
      let preveuh  =  match L.previous chain e_indx with
        | Nil ->  failwith "fptr chain corrupted, reached the top unexpectedly (investigate add_edge, and everything retaed to fptr maintainance / remove_from_chain"
        | P x  -> x
    in 
      set_range_end range i (preveuh) );

  L.remove chain e_indx);
  in 
  match mode with
  | "fptr" -> f g.frange g.fptr
  | "bptr" -> f g.brange g.bptr
  | _ -> failwith "invalid argument, should be fptr/bptr"


let delete_edge g e  = 
  if edge_in_graph g e then (
    let (V a, V b) = e in 
    let i,j = index_vertex g (V a) , index_vertex g (V b) in

    let findx  = see_edge_int_attribute g e "findx" in 
    let bindx  = see_edge_int_attribute g e "bindx" in

    let edge_index  = index_edge g e in 
    H.remove g.e e;
    D.remove g.edges edge_index;
      
    remove_from_chain g findx  i "fptr" ;
    remove_from_chain g bindx  j "bptr" 
  ) else failwith "Tried to delete edge but does not exists / delete_edge"



let veq = (=)

let eeq = (=)

let vcompare = Stdlib.compare

let vhash = H.hash

let ecompare = Stdlib.compare

let ehash = H.hash

let iter_ver f g = 
  H.iter (fun x _ -> f x) g.v

let iter_edg f g =
  if is_empty g then failwith "empty graph :  no edges /iter_edge" else
  H.iter (fun x _ -> f x) g.e

let fold_ver f g init = 
   H.fold (fun x _ z ->  f x z ) g.v init

let fold_edg f g init = 
  if is_empty g then failwith "empty graph :  no edges /fold_edg" else
 
  H.fold (fun x _ z ->  f x z ) g.e init


let vset_of_vertex_list ls  = Set ls

let iter_vset f s = 
  
  match s with
  | Set ls  -> List.iter f ls 
  | FRange ((u,v),a,edges) -> ( 
    let head  _ x = (*prends seulement le contenu de la case mais pas son index*)
      let (_,V j)  = (D.see edges x).edge in 
      f (V j)
    in 
    L.iterc_range head a u v )
  | BRange ((u,v),a,edges) -> (
    
    let tail  _ x =
      let (V i, _)  = (D.see edges x).edge in 
      f (V i)
    in
    L.iterc_range tail a u v; )

let adj_ver  g v mode  = 

  if not (vertex_in_graph g v ) then failwith "non existing vertex / adj_ver" ; 
  
  let f range chain constructor=
    let v_index  = index_vertex g v  in  
    let a,b  = see_range_begining range v_index ,  see_range_end range v_index in
      if a = -1 && b  = -1 then 
         (Set [])
    else
     constructor ((a,b),chain,g.edges)
  in 
  match mode with 
    | "prede" -> f g.brange g.bptr (fun (u,v,w) -> BRange (u,v,w))
    | "succ" ->f g.frange g.fptr (fun (u,v,w) -> FRange (u,v,w))
    | _ -> failwith "invalid argument, should be prede or succ"

let prede_ver g v = adj_ver g v "prede"

let succ_ver g v = adj_ver g v "succ"





(*et la on s'amuse avec les fonctions d'ordre sup UwU ;3*)

let fmake_edge i j :'a edge =  V i, V j 

let fadd_edge g i j  =  
  let e  =  fmake_edge i j in 
  add_edge g e

let fdelete_edge g i j  =  
  let e  =  fmake_edge i j in 
  delete_edge g e


let fast_op_vert g f = 
  fun v  -> let vert =  make_vertex v in f g vert 

let fast_op_edge g f =   
  fun  i j -> let e =  fmake_edge i j  in f g e 


let fast_mut_vert g f  = 
  fun  v x-> let vert =  make_vertex v  in f g vert x 

let fast_mut_edge g f = 
  fun  i j x-> let e =  fmake_edge i j  in f g e x 



let fsee_supply g = fast_op_vert g see_supply

let fsee_flow g  =  fast_op_edge g see_flow

let fsee_cost g  = fast_op_edge g see_cost

let fsee_capacity g = fast_op_edge g see_capacity

let fedge_in_graph g = fast_op_edge g edge_in_graph

let fset_flow g = fast_mut_edge g set_flow

let fset_cost g = fast_mut_edge g set_cost

let fset_capacity g = fast_mut_edge g set_capacity

let fset_supply g  = fast_mut_vert g set_supply


let fast_mut_edge2 g f  = 
  fun i j -> let e = fmake_edge i j in f g e

let fset_pos_infty_cost g = fast_mut_edge2 g (fun g e  -> set_pos_infty_cost g e)

let fset_neg_infty_cost g = fast_mut_edge2 g (fun g e  -> set_neg_infty_cost g e)

let fset_infty_capa g  =  fast_mut_edge2 g (fun g e  -> set_infty_capa g e)


