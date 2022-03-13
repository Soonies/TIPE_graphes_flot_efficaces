module D = Dynamic_array

exception Invalid_index of int*string

type pointer = |Nil | P of int



type 'a cell = {
  mutable prev : pointer ;
  mutable next : pointer ;
  mutable data : 'a
}


type 'a t = {
  mutable start : pointer ;
  mutable finish: pointer ;
  content: 'a cell D.t}


let create n  = 
  let newi  = {
    content = D.create n ;
    start = Nil;
    finish = Nil } 
  in 
  newi

let check_mem a k  = D.memi a.content k


let is_empty a  = (a.start = Nil)  

let start a  =  match a.start with
| Nil -> failwith "Tried to get start index but Empty linked_array/ start"
| P x-> x

let finish a  = match a.finish with
| Nil -> failwith "Tried to get end (finish) index but Empty linked_array/ finish"
| P x-> x
let size a = D.size a.content  

 (**[access_cell a k] returns the cell in the array [a] at index [k]*)
let access_cell a k  = 
    D.see a.content (k) 
 
let see a k  =  
  if not (check_mem a k) then raise (Invalid_index (k,"see")) else
  let x  = access_cell a k in 
  x.data

let previous a k =
  if not (check_mem a k) then raise (Invalid_index (k,"previous")) else
  let x = access_cell a k in 
  x.prev

let next a k =
  if not (check_mem a k) then raise (Invalid_index (k,"next")) else
  let x = access_cell a k in 
  x.next


  (**[modify_cell_pts a k field x] IF k<> -1 (is not nil) modifies to [x] the field [field] of the cell of the array [a] with index [k]. Nil stays untouched*)
let modify_cell_ptrs a k field x=
  (*etre sur que k est un bon indice de a svp*)
  let c = access_cell a k in 
  match field with
  | "next" ->   c.next <- x; 
  | "prev" ->   c.prev <- x;  
| _ -> failwith "wrong input; expected  field in : [next, prev]"

let modify_cell_data a k x=
  (*etre sur que k est un bon indice de a svp*)
  let c = access_cell a k in 
  c.data <- x


 (**[add a k x side] adds, depending on the "side" chosen (tail, head, insert), the element x after index k (index is only required for insert mode)*) 
let rec add a k x side =
  let n  = D.add a.content {prev = Nil ; next = Nil ; data = x} in (* n = index dans la dynamic array*) 
  
  
  if is_empty a then 
    if side = "insert" && k>0 then 
      raise (Invalid_index (k,"add"))
    else
      (a.start <- P n;
      a.finish <- P n;
      n)
  else
    
  let tail = finish a in 
  let head = start a in 
  
  match side with 
  | "tail" -> modify_cell_ptrs a tail "next" (P n); 
              modify_cell_ptrs a n "prev" (P tail);
              a.finish <- P n;
              n

  | "head" -> modify_cell_ptrs a head "prev" (P n); 
              modify_cell_ptrs a n "next" (P head);
              a.start <- P n;
              n

  | "insert" ->(  let next_k = next a k in match  next_k with
                    | Nil -> add a 694203 x "tail"
                    | P next_index -> (
                      modify_cell_ptrs a k "next" (P n);
                      modify_cell_ptrs a (next_index) "prev" (P n) ; 
                      modify_cell_ptrs a n "next" next_k;
                      modify_cell_ptrs a n "prev" (P k);
                      n)  
                )

  | _ ->  failwith "wrong key word, bla bla"
  

let tail_add a x  = 
  add a 694201 x "tail"
let head_add a x = 
  add a 694202 x "head"

let insert_add a k x = 
  if not (check_mem a k) then raise (Invalid_index (k,"insert_add")) else
  add a k x "insert"

let set a k x = 
  if not (check_mem a k) then raise (Invalid_index (k,"set")) else

  modify_cell_data a k x

let remove a k =
 if not (check_mem a k) then raise (Invalid_index (k,"remove")) else

  let prev = previous a k in
  let next = next a k in 

  let update_extremite_chaine ()  = 

    if a.start = P k then 
    a.start <- next ;
    if a.finish =P k then 
    a.finish <- prev
  in 
  update_extremite_chaine ();
  D.remove a.content (k) ; 

  match (prev, next) with
  | Nil , Nil -> ()
  | Nil, P x -> modify_cell_ptrs a x "prev" prev
  | P x , Nil -> modify_cell_ptrs a x "next" next
  | P x, P y -> modify_cell_ptrs a x "next" next;
                modify_cell_ptrs a y "prev" prev



let iterc_range f array strt stop=
  
  if not (check_mem array strt ) then  raise (Invalid_index (strt,"iterc_Range"))
  else if not ( check_mem array stop) then raise (Invalid_index (stop,"iterc_Range")) ;
  
  if not (is_empty array) then 
  let k = ref strt in
  let k_pre = ref (-2) in 

  while not  ( !k_pre = stop )do
    f !k (see array !k);
    k_pre := !k;
    let nexteuh = next array !k in 
    match  nexteuh with
    | Nil -> (
          if stop = finish array then (*reaching end of the array is normal*)
             () 
          else
             failwith " Reached the end of the linked list unexpectedly. iterc_range"
             )
    | P x ->  k:= x
    
  done
  
let iterc  f a =  iterc_range f a (start a) (finish a)

let foldc_left_range f a init strt stop = 
  let get_next a k = 
    let nexteuh  =  next a k  in 
    match  nexteuh with
      |Nil   -> failwith "Reached end of the linked list unexpectedly /  foldc_left_range"
      | P x -> x 
  in
  let rec aux  k   init   = 
    if k  =  stop then
      f k (see a k) init
    else 
      (let x  = f k (see a k) init in    
      aux (get_next a k)  x )
  in
  
  if not (check_mem a strt ) then  raise (Invalid_index (strt,"foldc_left_range"))
  else if not ( check_mem a stop) then raise (Invalid_index (stop,"foldc_left_range")) ;

  if not (is_empty a) then 
    aux strt init
  else
    init

let foldc_right_range  f a init strt stop = 
  let get_next a k = 
    let nexteuh  =  next a k  in 
    match  nexteuh with
      |Nil   -> failwith "Reached end of the linked list unexpectedly /  foldc_right_range"
      | P x -> x 
  in
  let rec aux k  = 
    if k = stop then
      f k (see a k) init
    else
      f k (see a k) (aux (get_next a k))
  in 

  if not (check_mem a strt ) then  raise (Invalid_index (strt,"foldc_right_range"))
  else if not ( check_mem a stop) then raise (Invalid_index (stop,"foldc_right_range")) ;


  if not (is_empty a) then
    aux strt 
  else 
    init

let foldc_left f a init = foldc_left_range f a init (start a) (finish a)

let foldc_right f a init = foldc_right_range f a init (start a) (finish a)


let show a  to_string= 
  let array   = ref "-{ " in
  let indixex = ref "->" in 
  let f  k x=
    array:= !array ^ to_string x^ "  ";  
    indixex:= !indixex ^ string_of_int k  ^"->" 
  in
  iterc f a ;
  print_endline !array;
  print_newline ();
  print_endline !indixex

