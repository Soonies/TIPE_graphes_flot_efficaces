let  mm = 2147483647
let u0  = 42


let ubar_cap =  999983
let ubars = Array.create ( ubar_cap+1) u0

let ()  =  for k =  1 to Array.length ubars -1 do
             ubars.(k) <- (ubars.(k-1) * 16807 + 17) mod mm
           done
let u n = ubars.(n mod 999983)

let q1 n = u n mod 997

let ss x y  =  (2*x+y) mod 997

let cc ls  =

  let rev_list  =  List.rev ls in

  let rec calcul ls = match ls with
    | [] -> 0
    | hd:: tl  -> ss (calcul tl) hd

  in
  calcul rev_list

let q2b =
  let ls  =  List.init (1000-100+1) (fun n ->
                 u (n+100))
  in
  cc ls

let q2c  =   let ls  =  List.init (1000000-999900+1) (fun n ->
                 u (n+999900))
  in
  cc ls

