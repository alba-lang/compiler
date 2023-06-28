type t

type istring = t

val value: t -> string

val compare: t -> t -> int


module Map:
sig
    type t

    val empty: t

    val get: string -> t -> istring * t
end
