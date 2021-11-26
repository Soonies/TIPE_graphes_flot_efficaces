(***********
Dynamic resizable 'a array*)

type 'a t (**0 indexed*)



(*****CONSTRUCTORS*)

(* val empty : 'a t The empty array (not implemented)*)

val fill : int-> 'a -> 'a t  (** fill n obj ->>> return an array of size n filled with (obj:'a) *)

val create : int-> 'a t (**[create n]   creates an empty int dynamic array of previsionnal length n . It is preferable to have n being within a precise ordre of magnitude of the final size of the array*)



(**OBSERVERS *)
val size : 'a t -> int (**return the number of elements contained in the array*)

val see: 'a t -> int -> 'a  (** see content of the ith  cell. An array is 0-indexed*)

(***MUTATORS**)

val add:  'a t -> 'a -> int (** [add a x] ->> add [x] to the array [a] and returns its index (pseudo random acces). The latter won't change until the element is eventually supressed from the array, in which case the newly freed space could be reattributed to a new element *)

val remove: 'a t -> int -> unit (**remove a k ->> removes the element of index [k] from the array [a]*)

val set : 'a t -> int -> 'a -> unit (**set a k x ->> replace the value at index k by x *)


(**ITERATORS*)

val iteri :  (int -> 'a -> unit ) -> 'a t -> unit (**[iteri f array] applies f to i and array.(i)  for each element *)

val iter : ('a -> unit) -> 'a t -> unit  (** iters through each element of a in an arbitrary order*)

val foldi_left : (int -> 'a ->  'b-> 'b) -> 'a t -> 'b -> 'b (**[flodi f  array x0 ] applies f to i annd array.(i)  and the previous result of the left fold *)

val fold_left : ('a ->  'b-> 'b) -> 'a t -> 'b -> 'b (**[flodi f  array x0 ] applies f to i annd array.(i)  and the previous result of the left fold *)


(**displayers*)
val show : 'a t -> ('a -> string) -> unit 