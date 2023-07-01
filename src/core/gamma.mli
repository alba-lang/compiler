type t

val count: t -> int

val empty: ?id:int -> unit -> t

val push: ?id:int -> Term.gen_binder -> t -> t
