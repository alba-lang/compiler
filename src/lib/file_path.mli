val normalize: char -> string -> string

val resolve: char -> string list -> string


val join: char -> string list -> string
(** [join sep paths]

    The string in the list [paths] are joined by inserting a separator between
    them. If the first element of the paths is the root (i.e. ["/"] on unix
    systems), then the separator is not duplicated.

    If all paths are normalized, then the result is normalized as well.
*)

val dirname: char -> string -> string

val basename: char -> string -> string
