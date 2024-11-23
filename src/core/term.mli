type t =
    int * t0

and t0 =
   | Sort of Sort.t
   | Meta of int


val prop: t
val any:  int -> t
val top:  int -> t
val meta: int -> t

val sort: Sort.t -> t

val up_by: t -> int

val up: int -> t -> t
