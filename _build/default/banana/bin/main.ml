let u0  =  42
let  us =  Array.make (int_of_float (10.**5.)) u0
let () =  
  for t=1 to (  Array.length us )-1 do
    us.(t)<- us.(t-1)*28 mod (336529) 
  done

let u n  =  us.(n)

