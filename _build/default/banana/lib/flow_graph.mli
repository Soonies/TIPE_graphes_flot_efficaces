    (*******************************************
        Interface of a `flow_graph`
        -Verticies are labeled by integers `int` starting from 1
        - flows and costs are integral
        -edges are tuples of vertices
        -Each edge can be capacitated (capacities are sealed)
        
        The module provides vertex-parths   `type vpath
                            edge-paths      `type epath
        
    *******************************************)
    
    
 

    
    type vertex  =int(**Vertices are labled by `int (pr print et pr les functeurs)*)

    type capacity = Capa of int | Infty

    type edge = vertex * vertex
 
    type t (** Flow Graph Type*)

    type vpath = vertex list (**A path within the graph described by vertices *)
    
    type epath = edge list  (**A path within the graph described by edges*)
    

    (**CAPACITY OPERATORS*)

  
    exception Unvalid_Capacity_Operation    (**Occurs when ... *)
    
    exception Uncapacitated_arc of edge (**Occurs when a capacity operation is called on an uncapacitated arc*)

    exception Not_residual_graph of t (**Occurs when a residual graph was expected*)
    

    val (++) : capacity -> capacity -> capacity(**Sum of two capacities*)
    
    val (--) : capacity -> capacity -> capacity (**Substraction of two capacities. *)
    
    val ( ** ) : capacity -> capacity -> capacity(**Multiplication of two capacities*)
    
     val (>>) : capacity -> capacity -> bool (**Tests is Capa1 > Capa2*)
   


    (*~SPECIAL VALUE CONSTRUCTORS~*)
    (**The Empty Graph*)
    val empty : t
    
    (**jsp???*)
    val upper_capacity :  int

    (*~CONSTRUCTORS~*)

    val create : int -> t (**Creates an n edged graph with 0 supply for all vertices and no edges. The size is FIXED and cannot be modified.*)
    
    val add_edge : t -> edge -> unit(** add_edge g e -> adds an edge e to the graph g*)
    
    val delete_edge : t -> edge -> unit(** delete_edge g e -> removes the edge e from the graph g*)
    
    

    (*~MUTATORS~*)
    
    val set_flow : t-> edge -> int -> unit(** set_flow g e x-> assigns the flow x to the edge e*)

    val set_cost : t -> edge -> int -> unit (** set_cost g e x-> assigns the cost x to the edge e *)

    val set_capacity : t-> edge -> capacity -> unit(** set_capacity g e x -> assigns the capacity x to the edge e*)

    val set_supply : t -> vertex -> int -> unit (** set_supply g v x-> assigns the supply x to the vertex v  *)


    

    (**iterators*)

    val iter_ver :  (vertex -> unit) ->  t -> unit(**iters though the vertices of the graph g applying `f*)
    
    val iter_edg : (edge -> unit) ->  t  -> unit (**iters through the edges of the graph g applying `f*)
    
    val fold_ver : ('a -> vertex -> 'a) -> 'a -> t ->  'a (** fold_ver f x0 g ->> (f...(f (f (f x0 v1) v2) v3) ... vn*)

    val fold_edge : ('a -> edge -> 'a) -> 'a -> t ->  'a (** fold_edge f x0 g ->> (f...(f (f (f x0 e1) e2) e3) ... en*)
   


    (*observers*)

    val size : t -> int(**size of the graph g ,i.e the number of vertices*)
    

    val nb_edge :  t -> int(** nb of edges of the graph g*)
    

    val veq : vertex -> vertex -> bool(**Tests equality of v1 and v2*)
    
    
    
    val vcompare : vertex -> vertex ->int(** standard compare function on the labels of the vertices *)

    val vhash : vertex -> int(**Hash function for the verticies*)
    

    val eeq : edge -> edge -> bool(**Tests equality of v1 and v2*)
    
    
    val ecompare : edge -> edge -> int(** standard compare function on the labels of the vertices *)
    
    
    val ehash : edge -> int(**Hash function for the edges*)
    

    
    (* all the folowing should be in O(1)*)

    val prede_ver : t -> vertex -> vertex list(**Lists the Predecessors of the vertex*)
    

    val succ_ver : t -> vertex -> vertex list(**Lists the succcessor of the vertex*)
    
    
    val see_name : t -> vertex -> string(**Returns the label of the vertex as a `string*)
    

    val see_supply : t -> vertex -> int (**Returns the supply  of the vertex as a `int*)
    

    val see_flow : t -> edge -> int(**Returns the flow  of the edge as a `int*)
    

    val see_cost : t-> edge -> int(**Returns the cost  of the edge as a `int*)
    
    
   val see_capacity : t -> edge -> capacity  (**Returns the capacity  of the edge as a `Capacity*)
    

    
    
    (*for residual graphs*)
    
    type residual_edge_type  = Forward | Backward
    
    val set_residuality : t -> bool -> unit 
    val is_residual : t -> bool                     (**Tests if the graph is residual*)
   
    val e_type : t -> edge -> residual_edge_type    (**Gives out the residual type of the edge (backward of foward)*)

            
    val add_fedge :  t ->  edge -> unit     (** add forward edge to a residual graph*)
    
     val add_bedge : t ->  edge -> unit  (** add backward edge to a residual graph*)

    
   (* transformations*)

    val sing_source_sing_sink :  t ->  t (**Tranforms a multi-source/sink graph to a single source/single sink graph *)
   
   val uncapacitated_to_capacitated :  t ->  unit (**Transformes an uncapacitated graph to a capacitated graph with infite capacities*)
