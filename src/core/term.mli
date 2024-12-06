open Std


type t =
    int * t0

and t0 =
   | Sort of Sort.t
   | Meta of int
   | Var  of Name.t
   | App  of t * t array
   | Lam  of int * varbind array * (t * t * t)
   | Pi   of int * varbind array * (t * t)
   | Ann  of t * t * t

and varbind = Info.Bind.t * t * t



val is_up_by: t -> int

val up: int -> t -> t

val count_implicits: int -> varbind array -> int


val prop: t
val any:  int -> t
val top:  int -> t
val meta: int -> t
val var:  int -> Name.t -> t

val sort: Sort.t -> t

val annotated: t -> t -> t -> t
val pi_sort: t -> t -> t
val pi1: (Info.Bind.t * t * t) -> (t * t) -> t
