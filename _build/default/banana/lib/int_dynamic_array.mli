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

val add:  t -> int -> unit (** [add a x] ->> add [x] to the array [a]*)

val remove: t -> int -> unit (**remove a k ->> removes the element of index [k] from the array [a]*)

val set : t -> int -> int -> unit (**set a k x ->> replace the value at index k by x *)


(**ITERATORS*)

val iter : (int -> unit) -> t -> unit  (** iters through each element of a*)

val fold : (int -> 'a -> 'a) ->  t -> 'a -> 'a  (**folds*)

