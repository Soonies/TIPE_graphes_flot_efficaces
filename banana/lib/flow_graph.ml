(***************************************)
(*Implementation selon la documentation papier*)
(*************)
module H = Hashtbl
module D  =  Dynamic_array
module L = Linked_array

type 'a vertex

type capacity  = Capa of int | Infty

type 'a edge = 'a vertex * 'a vertex

(** Flow graph represented by  : 
  - [v] :  Dict storing in an array every attributes of the vertex [vi] : []   *)
type 'a t = {
  v : ('a vertex, int array) H.t;
  e: ('a edge, int) H.t ;
  ptr : int L.t ;
  edges : (int array) D.t
  }

type 'a vset  = 