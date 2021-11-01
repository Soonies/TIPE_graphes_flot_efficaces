module type Container = 
  sig
    type 'a t 
    val create : unit ->  'a t
    val pop  : 'a t-> 'a
    val push : 'a t-> 'a -> unit
    val is_empty : 'a t -> bool
    val peek : 'a t -> 'a 
    val size : 'a t -> int
end

module Stack :  Container  = struct
  type 'a t = {mutable cont : 'a list}
  let create ()  =  {cont = []}
  let pop  s = let temp  =  List.hd s.cont in s.cont <- List.tl s.cont; temp 
  let push s x =  s.cont <- x :: s.cont 
  let is_empty s = s.cont = [] 
  let peek s = List.hd s.cont
  let size s  = List.length s.cont

end