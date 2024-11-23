type t =
   | Sort of Sort.t
   | Meta of int


val prop: t
val any:  int -> t
val top:  int -> t
val meta: int -> t
