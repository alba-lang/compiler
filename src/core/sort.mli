type t =
    | Prop
    | Any of int
    | Level
    | Top of int


val type_of: t -> t


val unify: bool -> t -> t -> bool

val pi: t -> t -> t
