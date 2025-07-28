type info =
    | Normal
    | Operator of Precedence.t
    | BracketEmpty
    | ParenEmpty
    | Meta
    | Wildcard

type t = info * string

val is_wildcard: t -> bool
val is_operator: t -> bool
val is_arrow:    t -> bool


val string: t -> string

val parens_string: t -> string
(** In case of an operator the name in parens. *)

val precedence: t -> Precedence.t

val compare: t -> t -> int

val normal: string -> t
val operator: string -> t
val operator_with_precedence: Precedence.t -> string -> t
val bracket_empty: string -> t
val paren_empty: string -> t
val meta: string -> t
val wildcard: t
