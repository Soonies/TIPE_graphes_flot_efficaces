module G =  Flow_graph

exception Uncapacitated_negcost_cycle
exception Break
val maximum_flow : G.t ->  G.t 
val neg_cycle : G.t -> G.epath