(***********
Dynamic resizable array*)

type t (*TRACE EST INUTILLLLEEEEE*)


(*****CONSTRUCTORS*)

val empty : t (**The empty array*)

val fill : int-> int-> t  (** fill n obj ->>> return an array of size n filled with (obj:int) *)

val create : int-> t (**[create n]   creates an empty int dynamic array of length n . It is preferable to have n being within a precise ordre of magnitude of the final size of the array*)


(**OBSERVERS *)
val size : t -> int (**return the number of elements contained in the array*)

val see: t -> int -> int  (** see content of the ith cell. An array is 0-indexed*)

(***MUTATORS**)

val add:  t -> int -> int (** [add a x] ->> add [x] to the array [a] and returns its index. The latter won't change until the element is eventually supressed from the array, in which case the newly freed space could be reattributed to a new element *)

val remove: t -> int -> unit (**remove a k ->> removes the element of index [k] from the array [a]*)

val set : t -> int -> int -> unit (**set a k x ->> replace the value at index k by x *)


(**ITERATORS*)

val iter : (int -> unit) -> t -> unit  (** iters through each element of a*)

val fold : (int -> 'a -> 'a) ->  t -> 'a -> 'a  (**folds*)

val enum : (int -> 'a -> unit ) -> t -> unit (**[enum f array] applies f to i and array.(i)  for each element *)

val fold_enum : (int -> int ->  'a) -> t -> 'a -> 'a (**[enum f  array x0 ] applies f to i annd array.(i)  and the previous result of the fold *)
