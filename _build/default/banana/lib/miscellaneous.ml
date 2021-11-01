let list_min ls =
    let rec  aux ls accu  = match ls with
    | []  -> accu
    | hd::tl ->   if hd < accu then aux tl hd
                  else aux tl accu
    in
    aux ls (List.hd ls) 

