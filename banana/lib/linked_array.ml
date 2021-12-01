module D = Dynamic_array

type 'a cell = {
  mutable prev : int ;
  mutable next : int ;
  mutable data : 'a
}

type 'a t ={mutable start : int ; mutable finish: int ; content: 'a cell D.t}


let create n = {
  content = D.create n ;
  start = -1;
  finish = -1;
}

let start a  =  a.start

let finish a  = a.finish

let is_empty a  = (start a = -1)  

let size a = D.size a.content

 (**[access_cell a k] returns the cell in the array [a] at index [k]*)
let access_cell a k  = 
  D.see a.content k

let see a k  =  
  let x  = access_cell a k in 
  x.data
let previous a k =
  let x = access_cell a k in 
  x.prev

let next a k =
  let x = access_cell a k in 
  x.next


  (**[modify_cell_pts a k field x] modifies the field [field] of the cell of the array [a] with index [k]*)
let modify_cell_ptrs a k field x=
  let c = access_cell a k in 
  match field with
  | "next" ->   c.next <- x;  D.set a.content k c 
  | "prev" ->   c.prev <- x;  D.set a.content k c 
| _ -> failwith "wrong input; expected  field in : [next, prev, data]"

let modify_cell_data a k x=
  let c = access_cell a k in 
  c.data <- x;  
  D.set a.content k c 


 (**[add a k x side] adds, depending on the "side" chosen (tail, head, insert), the element x at index k (index is only required for insert mode)*) 
let add a k x side =

  let n  = D.add a.content {prev = -1 ; next = -1 ; data = x} in 
  let tail = finish a in 
  let head = start a in 
  let prev_k = previous a k in 
  let next_k = next a k in
  
  if is_empty a then 
    if k>0 then 
      failwith "Index out of bound"
    else
      (a.start <- n;
      a.finish <-n;
      n)
  else

  match side with 
  | "tail" -> modify_cell_ptrs a tail "next" n; 
              modify_cell_ptrs a n "prev" tail;
              a.finish <- n;
              n

  | "head" -> modify_cell_ptrs a head "prev" n; 
              modify_cell_ptrs a n "next" head;
              a.start <- n;
              n

  | "insert" ->   modify_cell_ptrs a prev_k "next" n;
                  modify_cell_ptrs a next_k "prev" n ; 
                  modify_cell_ptrs a n "next" next_k;
                  modify_cell_ptrs a n "prev" prev_k;
                  n

  | _ ->  failwith "wrong key word, bla bla"
  

let tail_add a x  = 
  add a 0 x "tail"
let head_add a x = 
  add a 0 x "head"

let insert_add a k x = 
  add a k x "insert"

let set a k x = 
  modify_cell_data a k x

let remove a k =
  let prev = previous a k in
  let next = next a k in 
  D.remove a.content k ;
  modify_cell_ptrs a prev "next" next;
  modify_cell_ptrs a next "prev" prev

let iterc_range f a strt stop=
  if not (is_empty a) then 
  let k = ref strt in
  let k_pre = ref (-2) in 

  while not (!k_pre = stop) do
    f !k (see a !k);
    k_pre := !k;
    k:= next a !k 
  done

let iterc  f a =  iterc_range f a (start a) (finish a)

let foldc_left_range f a init strt stop = 
  let rec aux  k   init   = 
    if k  =  stop then
      f k (see a k) init
    else 
      (let x  = f k (see a k) init in    
      aux (next a k)  x )
  in
  if not (is_empty a) then 
    aux strt init
  else
    init

let foldc_right_range  f a init strt stop = 
  let rec aux k  = 
    if k = stop then
      f k (see a k) init
    else
      f k (see a k) (aux (next a k))
  in 
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
    array:= !array ^ to_string x^"  ";  
    indixex:= !indixex ^ string_of_int k  ^"->" 
  in
  iterc f a ;
  print_endline !array;
  print_newline ();
  print_endline !indixex

