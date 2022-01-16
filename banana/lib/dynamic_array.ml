(************************)
(*   we use disctionnaries to implement
a resizable array 

if A is an array containing a0, a1 ... an |]
  A will be represented by a record {next_index = int stack ;  {0 : a_0 ; 1 : a_1 ; ... ; n : a_n}}*)

module H = Hashtbl
module S = Stack

type 'a t  = {next_index : int S.t ; tbl : (int,'a) H.t } (** next_index indicates the index of the next element 
to be added, tbl is the hash table representing the array*)


exception Invalid_index of int


let fill n x  = 
  let neww = {next_index= S.create () ; tbl = H.create n} in 
  S.push n neww.next_index;
  for k= 0  to n-1 do 
    H.add neww.tbl k x
  done;
  neww

let create n =  
  let newi = {next_index= S.create () ; tbl = H.create n} in 
  S.push 0 newi.next_index;
  newi
let size a = H.length a.tbl

let is_empty a  = size a = 0 

let see a k = 
   try
  H.find a.tbl k 
with
  |Not_found ->  raise (Invalid_index k )   


let get_1_elmnt a = 
  let resultat  = ref [] in 

  let f x y _= 
    resultat := (x,y):: !resultat ; 
    failwith "fini"

  in
  (  try  
    H.fold f a.tbl ()
  with
    |  Failure _ -> () )
  ;  !resultat |> List.hd |> snd


(**adds the new element then updates [next_index] to its new value (depending on whether we have just filled a free pace or if we are at the end of the array)*)
let add a x = 
  let length  =  size a in
  let i= S.pop  a.next_index in 
  H.add a.tbl i x;
  if S.is_empty a.next_index then
    S.push (length +1 ) a.next_index
  ;
  i

(**updates [next_index] to point to the newly free space then frees up the space*)
let remove a k = 
  S.push k a.next_index  ;
  H.remove a.tbl k

let set a k x =  H.replace a.tbl k x

let iter f a = H.iter (fun _ x -> f x) a.tbl (*f ignores the index of the item*)

let iteri f  a  = 
  H.fold (fun  a b _ -> f a b) a.tbl ()

let foldi_left  f a p = H.fold f a.tbl p

let fold_left f a x =  foldi_left (fun _ a b -> f a b) a x

let show a  to_string= 
  let array   = ref "{[ " in
  let indixex = ref "   " in 
  let f  k x=
    array:= !array ^ to_string x^"    ";  
    indixex:= !indixex ^ string_of_int k  ^"    " 
  in
  iteri f a ;
  print_endline !array;
  print_newline ();
  print_endline !indixex

