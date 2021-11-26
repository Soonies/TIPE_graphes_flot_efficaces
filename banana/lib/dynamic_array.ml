(************************)
(*     for the time being, we use disctionnaries to implement
a resizable array 

if A is an array containing a0, a1 ... an |]
  A will be represented by a record {next_index = n ;  {0 : a_0 ; 1 : a_1 ; ... ; n : a_n}}*)

module H = Hashtbl
module S = Stack

type 'a t  = {mutable next_index : int ; tbl : (int,'a) H.t } (** nnext_index indicates the index of the next element 
to be added, tbl is the hash table representing the array*)

(* let empty =  { next_index = 0 ; tbl  = H.create 20}*)

let fill n x  = 
  let neww = {next_index= n ; tbl = H.create n} in 
  for k= 0  to n-1 do 
    H.add neww.tbl k x
  done;
  neww

let create n = {next_index= 0 ; tbl = H.create n}

let size a = H.length a.tbl

let see a k = H.find a.tbl k (*Note to self: dont forget to add excetpion*)

(**adds the new element then updates [next_index] to its new value (depending on whether we have just filled a free pace or if we are at the end of the array)*)
let add a x = 
  let n = size a in 
  let next =  a.next_index in
  H.add a.tbl a.next_index x;
  if n =  next then 
    a.next_index <- a.next_index +1 
  else
    a.next_index <- n
  ;
  next

(**updates [next_index] to point to the newly free space then frees up the space*) (*****************************************************************)
let remove a k = 
  a.next_index <- k ;
  H.remove a.tbl k

let set a k x = H.replace a.tbl k x

let iter f a = H.iter (fun _ x -> f x) a.tbl (*f ignores the index of the item*)

let iteri f  a  = 
  H.fold (fun  a b _ -> f a b) a.tbl ()

let foldi_left  f a (p:'b) = H.fold f a.tbl p

let fold_left f a x =  foldi_left (fun _ a b -> f a b) a x

let show a  to_string= 
  let array   = ref "{[ " in
  let indixex = ref "   " in 
  let f  k x=
    array:= !array ^ to_string x^"  ";  
    indixex:= !indixex ^ string_of_int k  ^"  " 
  in
  iteri f a ;
  print_endline !array;
  print_newline ();
  print_endline !indixex
