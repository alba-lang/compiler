type t =
    int * t0

and t0 =
   | Sort of Sort.t
   | Meta of int
   | Pi of int * (Info.Bind.t * t * t) array * (t * t)


val is_up_by: t -> int

val up: int -> t -> t


val prop: t
val any:  int -> t
val top:  int -> t
val meta: int -> t

val sort: Sort.t -> t

val pi_sort: t -> t -> t

val pi: (Info.Bind.t * t * t) array -> (t * t) -> t
