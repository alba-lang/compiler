(** String of unicode characters in utf8 encoding

    Strings of unicode characters are normal ocaml strings with information
    about the number of unicode characters in the string and if all are valid
    unicode encodings.
*)

type t

val string: t -> string

val is_valid: t -> bool

val length: t -> int

val byte_length: t -> int

val foldi: ('a -> int -> Uchar.t -> 'a) -> 'a -> t -> 'a

val fold: ('a -> Uchar.t -> 'a) -> 'a -> t -> 'a

val (^): t -> t -> t

val make: string -> t
