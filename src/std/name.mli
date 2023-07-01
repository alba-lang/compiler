type info =
    | Normal
    | Operator of Precedence.t
    | BracketEmpty
    | ParenEmpty
    | Meta
    | Wildcard

type t = info * string

val is_wildcard: t -> bool
val compare: t -> t -> int

val normal: string -> t
val operator: string -> t
val bracket_empty: string -> t
val paren_empty: string -> t
val meta: string -> t
val wildcard: string -> t
