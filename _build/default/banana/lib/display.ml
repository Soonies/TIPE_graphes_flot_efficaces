module G = Flow_graph

module type X_graph = sig
 val input_g : int G.t
end 

module DisplayFunct (M:X_graph) = 
(*obj :  writing a function that generates a dot file of a g:G.t graph*)
  struct
    let g0 = M.input_g
    let get_vertex_label g (v: int G.vertex) = 
      let supply = string_of_int (G.see_supply g v) in
       
       string_of_int (G.see_label v) ^ ": b"^supply

    let get_edge_label g (e: int G.edge) = 
      let capa = match G.see_capacity g e with 
                  | Infty -> "/Infty"
                  | Capa x ->"/" ^ (string_of_int x)
      in
      string_of_int (G.see_flow g e ) ^ capa ^"~"^(string_of_int (G.see_cost g e))


    module Ver  = struct
      type t = int  G.vertex
    end
    module Edg = struct
      type t = int G.edge
      let src ((i,_): int G.edge) = i 
      let dst ((_,j): int G.edge) = j  
    end

    module X = struct
      type t = int  G.t
      module V = Ver
      module E = Edg
      let iter_vertex = G.iter_ver 
      let iter_edges_e = G.iter_edg

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      
      let vertex_name  v  = string_of_int  (G.see_label v)
      let vertex_attributes v = [ `Label (get_vertex_label g0 v)] 
      let get_subgraph _ = None 
      let default_edge_attributes _ = []
      let edge_attributes e = [`Label (get_edge_label g0 e ); (* `Penwidth (float_of_int (G.see_flow g0 (i,j))) *)]

    end
    
   
    module Print = Graph.Graphviz.Dot (X)
    let generate dot_name = 
      flush_all ();
      let file = open_out dot_name in
      let _ = Print.output_graph file g0 in 
      close_out file 
    
    let convert_dot_to_png dot_name png_name = 
      let dot_conv  = "dot -Tpng "^dot_name^" -o ./Images/"^png_name in 
      let _ = Sys.command dot_conv in
      let move  =  "mv "^dot_name^" Images" in 
      let _  =  Sys.command move in  
      ()
  end 
