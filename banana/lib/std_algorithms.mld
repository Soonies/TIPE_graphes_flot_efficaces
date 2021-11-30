module G = Flow_graph
 
  exception Uncapacitated_negcost_cycle
  exception Break
  (*open Stack
  let dft g f  ?goal:(goal= -1) (v0:vertex)  = 
    let n =size g  in  
    let prede_ver =  Array.make n (-1) in 
    let ancestor =  ref (-1) in  
    let to_come  = Stack.create ()  in
    push to_come v0 ; 

  try
    while not (is_empty to_come ) do 
      let i = pop to_come in 
      f i ;
      prede_ver.(i) <- !ancestor; 
      ancestor := i ;
      List.iter (fun x -> push to_come x) (succ_ver g i);
      
      if i =  goal then raise Break

      done;
    
    prede_ver
  with
    | Break -> prede_ver*)
  let maximum_flow _ =  G.create 0
  let neg_cycle _ = []
