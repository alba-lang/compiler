type 'a t


val length: 'a t -> int


val make: unit -> 'a t

val get: 'a t -> int -> 'a

val set: 'a t -> int -> 'a -> unit

val push: 'a t -> 'a -> unit
