type t = Token_type.t * string

val to_string: t -> string
val to_escaped: t -> string

val paren_left: t
val paren_right: t
val paren_empty: t
val bracket_left: t
val bracket_right: t
val bracket_empty: t
val brace_left: t
val brace_right: t

val assign: t
val colon: t
val comma: t
val dot: t
val lambda: t
val semicolon: t
