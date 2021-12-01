(*******************************************
    Interface of a `flow_graph`
    - flows and costs are integral
    -edges are tuples of vertices
    -Each 'a edge can be capacitated (capacities are sealed)
    
    The module provides 'a vertex-parths   `type vpath
                        'a edge-paths      `type epath
    
*******************************************)





type 'a vertex  (**Vertices are labled by `int (pr print et pr les functeurs)*)

type capacity = Capa of int | Infty

type 'a edge = 'a vertex * 'a vertex

type 'a flow = ('a edge -> int) 

type 'a t (** Flow Graph Type, with 'a vertices*)

type 'a vset (**a set of vertices*)

type 'a eset (**a set of edges *)


(**CAPACITY OPERATORS*)


exception Unvalid_Capacity_Operation    (**Occurs when ... *)

exception Uncapacitated_arc of (string) (**Occurs when a capacity operation is called on an uncapacitated arc*)

exception Not_residual_graph of string(**Occurs when a residual graph was expected*)


val (++) : capacity -> capacity -> capacity(**Sum of two capacities*)

val (--) : capacity -> capacity -> capacity (**Substraction of two capacities. *)

val ( ** ) : capacity -> capacity -> capacity(**Multiplication of two capacities*)

val (>>) : capacity -> capacity -> bool (**Tests is Capa1 > Capa2*)



(*~SPECIAL VALUE CONSTRUCTORS~*)
(**The Empty Graph*)
val is_empty : 'a t ->  bool


(*~CONSTRUCTORS~*)

val create : int -> 'a t (**Creates an n edged graph with 0 supply for all vertices and no edges. The size is FIXED and cannot be modified.*)

val add_edge : 'a t -> 'a edge -> unit(** add_edge g e -> adds an 'a edge e to the graph g*)

val delete_edge : 'a t -> 'a edge -> unit(** delete_edge g e -> removes the 'a edge e from the graph g*)

(*OBSERVERS*)

val size : 'a t -> int(**size of the graph g ,i.e the number of vertices*)

val nb_edge :  'a t -> int(** nb of edges of the graph g*)

val veq : 'a vertex -> 'a vertex -> bool(**Tests equality of v1 and v2*)

val eeq : 'a edge -> 'a edge -> bool(**Tests equality of v1 and v2*)

    
val vcompare : 'a vertex -> 'a vertex ->int(** standard compare function on the labels of the vertices *)

val vhash : 'a vertex -> int(**Hash function for the verticies*)

val ecompare : 'a edge -> 'a edge -> int(** standard compare function on the labels of the vertices *)

val ehash : 'a edge -> int(**Hash function for the edges*)

val see_name : 'a t -> 'a vertex -> string(**Returns the label of the 'a vertex as a `string*)


val see_supply : 'a t -> 'a vertex -> int (**Returns the supply  of the 'a vertex as a `int*)


val see_flow : 'a t -> 'a edge -> int(**Returns the flow  of the 'a edge as a `int*)


val see_cost : 'a t-> 'a edge -> int(**Returns the cost  of the 'a edge as a `int*)


val see_capacity : 'a t -> 'a edge -> capacity  (**Returns the capacity  of the 'a edge as a `Capacity*)






(*~MUTATORS~*)

val set_flow : 'a t-> 'a edge -> int -> unit(** set_flow g e x-> assigns the flow x to the 'a edge e*)

val set_cost : 'a t -> 'a edge -> int -> unit (** set_cost g e x-> assigns the cost x to the 'a edge e *)

val set_capacity : 'a t-> 'a edge -> capacity -> unit(** set_capacity g e x -> assigns the capacity x to the 'a edge e*)

val set_supply : 'a t -> 'a vertex -> int -> unit (** set_supply g v x-> assigns the supply x to the 'a vertex v  *)




(**iterators*)

val iter_ver :  ('a vertex -> unit) ->  'a t -> unit(**iters though the vertices of the graph g applying `f*)

val iter_edg : ('a edge -> unit) ->  'a t  -> unit (**iters through the edges of the graph g applying `f*)

val fold_ver : ('a  vertex -> 'b-> 'b) ->  'a  t ->'b ->  'b (** fold_ver f x0 g ->> (f...(f (f (f x0 v1) v2) v3) ... vn*)

val fold_edge : ('a -> 'b  edge -> 'a) -> 'a -> 'b  t ->  'a (** fold_edge f x0 g ->> (f...(f (f (f x0 e1) e2) e3) ... en*)


(**vset and eset iterators*)

val iter_vset : ('a vertex -> unit) -> 'a vset -> unit  

val iter_eset : ('a edge -> unit) -> 'a eset -> unit  




val prede_ver : 'a t -> 'a vertex -> 'a vertex list(**Lists the Predecessors of the 'a vertex*)


val succ_ver : 'a t -> 'a vertex -> 'a vertex list(**Lists the succcessor of the 'a vertex*)
