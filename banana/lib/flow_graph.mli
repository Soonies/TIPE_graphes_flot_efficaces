(*******************************************
    Interface of a `flow_graph`
    - flows and costs are integral
    -vertices start at v=1
    -edges are tuples of vertices
    -Each 'a edge can be capacitated (capacities are sealed)
    
    The module provides 'a vertex-parths   `type vpath
                        'a edge-paths      `type epath
    
*******************************************)





type 'a vertex  (**Vertices are labled by `int (pr print et pr les functeurs)*)

type capacity = Capa of int | Infty 

type 'a edge = 'a vertex * 'a vertex

type 'a t (** Flow Graph Type, with 'a vertices*)

type 'a vset (**a set of vertices*)


(**CAPACITY OPERATORS*)


exception Invalid_Capacity_Operation    (**Occurs when ... *)

exception Calculating_Neg_Capacity

exception Uncapacitated_arc of (string) (**Occurs when a capacity operation is called on an uncapacitated arc*)

exception Not_residual_graph of string(**Occurs when a residual graph was expected*)


val (++) : capacity -> capacity -> capacity(**Sum of two capacities*)

val (--) : capacity -> capacity -> capacity (**Substraction of two capacities. A CAPACITY CANNOT BE NEGATIVE . @raises [Calculating_Neg_Capacity] if substractiong non-compatible capacities *)

val ( ** ) : capacity -> capacity -> capacity(**Multiplication of two capacities*)

val (<<) : capacity -> capacity -> bool (**Tests is Capa1 > Capa2*)



(*~SPECIAL VALUE CONSTRUCTORS~*)
(**The Empty Graph*)
val is_empty : 'a t ->  bool (*an empty graph is a graph with no edge*)



(*OBSERVERS*)

val size : 'a t -> int(**size of the graph g ,i.e the number of vertices*)

val nb_edge :  'a t -> int (** nb of edges of the graph g*)

val see_label : 'a vertex -> 'a

val see_supply : 'a t -> 'a vertex -> int (**Returns the supply  of the 'a vertex as a `int*)

val see_flow : 'a t -> 'a edge -> int(**Returns the flow  of the 'a edge as a `int*)

val see_cost : 'a t-> 'a edge -> int(**Returns the cost  of the 'a edge as a `int*)

val see_capacity : 'a t -> 'a edge -> capacity  (**Returns the capacity  of the 'a edge as a `Capacity*)

(*~MUTATORS~*)

val set_flow_edge : 'a t-> 'a edge -> int -> unit(** set_flow g e x-> assigns the flow x to the 'a edge e*)

val set_cost : 'a t -> 'a edge -> int -> unit (** set_cost g e x-> assigns the cost x to the 'a edge e *)

val set_capacity : 'a t-> 'a edge -> capacity -> unit(** set_capacity g e x -> assigns the capacity x to the 'a edge e*)

val set_supply : 'a t -> 'a vertex -> int -> unit (** set_supply g v x-> assigns the supply x to the 'a vertex v  *)



(*~CONSTRUCTORS~*)

val create : 'a list -> 'a t (**[create ls] Creates a graph with the vertices of the list [ls] all with 0 supply, and no edges.    Theset of vertices is FIXED and cannot be extended, modified nor suppressed.*)

val make_vertex :  'a -> 'a vertex

val make_edge : 'a vertex  -> 'a vertex -> 'a edge  

val add_edge : 'a t -> 'a edge -> unit(** add_edge g e -> adds an 'a edge e to the graph g*)

val add_edge_list : 'a t -> 'a edge list -> unit

val delete_edge : 'a t -> 'a edge -> unit(** delete_edge g e -> removes the 'a edge e from the graph g*)



(*TESTS and STD*)
val veq : 'a vertex -> 'a vertex -> bool(**Tests equality of v1 and v2*) (*useless???*)

val eeq : 'a edge -> 'a edge -> bool(**Tests equality of e1 and e2*) (*useless???*)

    
val vcompare : 'a vertex -> 'a vertex ->int(** standard compare function on the labels of the vertices *)

val vhash : 'a vertex -> int(**Hash function for the verticies*)

val ecompare : 'a edge -> 'a edge -> int(** standard compare function on the labels of the vertices *)

val ehash : 'a edge -> int(**Hash function for the edges*)




(**iterators*)

val iter_ver :  ('a vertex -> unit) ->  'a t -> unit(**iters though the vertices of the graph [g] applying [f]*)

val iter_edg : ('a edge -> unit) ->  'a t  -> unit (**iters through the edges of the graph g applying `f*)

val fold_ver : ('a  vertex -> 'b-> 'b) ->  'a  t ->'b ->  'b (** fold_ver f x0 g ->> (f...(f (f (f x0 v1) v2) v3) ... vn*)

val fold_edg : ('a edge -> 'b   -> 'b) -> 'a  t -> 'b ->  'b (** fold_edge f x0 g ->> (f...(f (f (f x0 e1) e2) e3) ... en*)


(**vset and eset iterators*)
val vset_of_vertex_list : 'a vertex list -> 'a vset 

val iter_vset : ('a vertex -> unit) -> 'a vset -> unit  

val prede_ver : 'a t -> 'a vertex -> 'a vset(**Lists the Predecessors of the 'a vertex*)

val succ_ver : 'a t -> 'a vertex -> 'a vset(**Lists the succcessor of the 'a vertex*)



(*#fast observers constructors and mutators and  #*)


val fmake_edge :  'a  -> 'a  -> 'a edge (*f stands for "fast"*)

val fadd_edge :  'a t -> 'a -> 'a -> unit (** [ fadd_edge g i j ] make an edge of i  and j *)



val fsee_supply : 'a t -> 'a  -> int (**Returns the supply  of the 'a vertex as a `int*)

val fsee_flow : 'a t -> 'a  -> 'a  -> int(**Returns the flow  of the 'a edge as a `int*)

val fsee_cost : 'a t-> 'a  -> 'a  -> int(**Returns the cost  of the 'a edge as a `int*)

val fsee_capacity : 'a t -> 'a  -> 'a ->  capacity  (**Returns the capacity  of the 'a edge as a `Capacity*)





val fset_flow_edge : 'a t-> 'a  -> 'a ->  int -> unit(** set_flow_edge g e x-> assigns the flow x to the 'a edge e*)

val fset_cost : 'a t -> 'a  -> 'a -> int -> unit (** set_cost g e x-> assigns the cost x to the 'a edge e *)

val fset_capacity : 'a t-> 'a  -> 'a -> capacity -> unit(** set_capacity g e x -> assigns the capacity x to the 'a edge e*)

val fset_supply : 'a t -> 'a -> int -> unit (** set_supply g v x-> assigns the supply x to the 'a vertex v  *)



