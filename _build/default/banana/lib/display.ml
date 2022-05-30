module P = Plplot
module G = Flow_graph

module type Graph = sig
 type a 
  val input_g : a G.t
 
 val string_of_a : a -> string
 
end 

module DisplayFunct (M:Graph) = 
(*obj :  writing a function that generates a dot file of a g:G.t graph*)
  struct
    let g0 = M.input_g
    let f  = M.string_of_a 
    let get_vertex_label g v = 
      let supply = string_of_int (G.see_supply g v) in
       
       f (G.see_label v) ^ ": b"^supply

    let get_edge_label g e = 
      let capa = let u  =  G.see_capacity g e  in 
                  if G.is_infty_capa g u then "/Infty"
                  else "/" ^ (string_of_int u)
                
      in
      let c  =  G.see_cost g e  in
      string_of_int (G.see_flow g e ) ^ capa ^"~"^(
        ( if G.is_pos_infty_cost g  c then 
            "+Infty" 
          else if G.is_neg_infty_cost g c then
             "-Infty"  else string_of_float c ))


    module Ver  = struct
      type t = M.a G.vertex
    end
    module Edg = struct
      type t = M.a G.edge
      let src ((i,_): M.a G.edge) = i 
      let dst ((_,j): M.a G.edge) = j  
    end

    module X = struct
      type t = M.a  G.t
      module V = Ver
      module E = Edg
      let iter_vertex = G.iter_ver 
      let iter_edges_e = G.iter_edg

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      
      let vertex_name  v  = f  (G.see_label v)
      let vertex_attributes v = [ `Label (get_vertex_label g0 v)] 
      let get_subgraph _ = None 
      let default_edge_attributes _ = []
      let edge_attributes e = [`Label (get_edge_label g0 e ); `Penwidth (float_of_int (G.see_flow g0 e)) ]

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

module type TempGraph = sig
  val input_g : (int*int) G.t
 
 val string_of_a : int*int -> string
 
end 

module DisplayPlane (M: TempGraph  ) = struct
  let g0 = M.input_g

  let f  =  float_of_int 

  
  let scale_of_smth  g iterf watchf sample= 

    let max_supply = ref sample in 
    let min_supply = ref sample in

    let update_max_min v =
      let supp =  watchf g v in 
      if supp > !max_supply then
        max_supply := supp
      ;
      if supp < !min_supply then
        min_supply := supp
    
    in
    iterf update_max_min g  ;

    (fun  x ->
      let b  = f !max_supply in
      let a  = f !min_supply in
      1. /. (b -. a) *. x -. a /. (b -.a))
    
  let get_borders g  = 
    let rnd =  g |>  G.get_1_vert |> G.see_label |> fst |> f in  
    let xmi = ref rnd in
    let xma = ref rnd in
    let ymi = ref rnd in
    let yma = ref rnd in
  
    let parse v  = 

      let x,y =  G.see_label v in
      let x,y = f x, f y in   
      
      if  !xmi >  x then 
        xmi:= x;
      if  !ymi >  y then 
        ymi:= y;
      if  !xma <  x then 
        xma:= x;
      if  !yma <  y then 
        yma:= y;
    in
    G.iter_ver parse g ; 
    let ygap  =  0.2 *.(!yma -. !ymi) in 
    let xgap  = 0.2*.(!xma -. !xmi) in
    (!xmi -. xgap , !xma +. xgap , !ymi -. ygap, !yma+. ygap)

  (**g is supposed to verify the balance property, with at least one  non zero supply, doit aussi renvoyer le min des coordonnees*)
  let parse_vertices g =
    
    let supply_scale = scale_of_smth g G.iter_ver (G.see_supply) (G.get_1_supply g) in 

    let parse_vert v  = 
      let x,y =  G.see_label v in
      let x,y = f x, f y in   
      v |> G.see_supply g |> f |>  supply_scale |> P.plcol1 ;
      P.plschr 0. 2.5 ;
      P.plstring [|x|] [|y|] "•"

    in 
    G.iter_ver parse_vert g
      
  let parse_edges g  = 
    
    let flow_scale  = scale_of_smth g G.iter_edg G.see_flow (G.get_1_flow g ) in 
    let parse_edg  e  = 
      let i,j  =  e in 
      let i,j  =  G.see_label  i , G.see_label j  in 
      let x1,y1 = i in 
      let x2,y2  = j  in 

      let flow = G.see_flow g e  in 
      if flow = 0 then () else(
      let ratio = (flow |> f |> flow_scale) in
      P.plwidth (ratio*. 10.) ; 
      P.plcol1 ratio;
      P.pljoin (f x1) (f y1) (f x2) (f y2);
      P.plstring [|f x2|] [| f y2|] "|>")
    in
    G.iter_edg parse_edg g 

  
  let r  =  Array.init 256 (fun x->if x >= 128 then 255 else 2*x)
  let g  = Array.init 256 (fun x-> max (2*(x-128)) 0) 
  let b  =   Array.init 256 (fun x->max (255 - (2*x)) 0)


  let make_image () = 
    P.plsdev "pngqt" ;
    P.plsfnam "yasss.png" ;
    P.plinit () ; 
    P.plscmap1 r g b ; 
    let xmin, xmax, ymin, ymax = get_borders g0 in 
    P.plenv xmin xmax ymin ymax 0 0 ;
    P.pllab "x" "y" "Graph representation" ;
    parse_edges g0;
    parse_vertices g0;
    P.plend ()
end