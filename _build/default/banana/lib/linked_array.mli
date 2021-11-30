(***********************************************************)
(** Array coupled with a doubled linked list structure**)
(************************************************************)

type 'a t


val create : int -> 'a t (** creates a new empty linked array of previsionnal length n *)

val is_empty :  'a t -> bool

(**OBSERVERS *)
val size : 'a t -> int (**return the number of elements contained in the array*)


val start: 'a t -> int (**index of the start of the chain*)

val finish : 'a t -> int (**index of the end/finishing of the chain. Not called "end" just bc lol*)

val see: 'a t -> int -> 'a  (** see content of the ith  cell. An array is 0-indexed*)

val previous :  'a t -> int -> int  (** [previous a k] index of the predecessor to the k-ith element of the array [a]*)

val next : 'a t -> int -> int  (** [next a k] index of the successor to the k-ith element of the array [a]*)

(***MUTATORS**)

val tail_add:  'a t -> 'a -> int (** [tail_add a x] ->> add [x] to the array [a] at the end of the chain, and returns its index (pseudo random acces). The latter won't change until the element is eventually supressed from the array, in which case the newly freed space could be reattributed to a new element *)

val head_add : 'a t -> 'a -> int (** [head_add a x ] ->> add [x] to the array [a] at the begining of the chain, and returns its index (pseudo random acces). The latter won't change until the element is eventually supressed from the array, in which case the newly freed space could be reattributed to a new element *)

val insert_add : 'a t ->  int ->'a -> int (** [head_add  k a x ] ->> add [x] to the array [a] by  placing [x] directly after the element at index [k] in the chain , and returns its index (pseudo random acces). The latter won't change until the element is eventually supressed from the array, in which case the newly freed space could be reattributed to a new element *)

val set : 'a t -> int -> 'a -> unit (**[set a k x] ->> replace the value at index [k] by [x], while conserving the initial chain *)

val remove: 'a t -> int -> unit (**[remove a k x] ->> replace the value at index [k] by [x], and bridge the gap with the previous and next element*)

(**ITERATORS AND FOLDS*) 

val iterc_range : ( int -> 'a -> unit) -> 'a t -> int ->  int -> unit  (**[iterc f a strt stop] iters through the chain [a] from index start to stop*) (*pas de sens d'avoir des iter sur les indices slm pcq le seul ordre defini c'est celui de la chaine...*)

val iterc : ( int->'a -> unit) -> 'a t ->  unit  (**[iterc f a strt stop] iters through the chain from index start to stop, applying f to a.[i]*)

val foldc_left_range : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b ->int->int-> 'b (** Folds on the chain *)

val foldc_right_range : (int ->'a -> 'b -> 'b) -> 'a t -> 'b->int -> int -> 'b (** Folds on the chain *)


val foldc_left : (int ->'a -> 'b -> 'b) -> 'a t -> 'b -> 'b (** Folds on the chain *)

val foldc_right : (int  -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b (** Folds on the chain *)



(**Display*)

val show : 'a t  -> ('a -> string) -> unit