type c_hole =
    | Infer_type of string * Fmlib_parse.Position.range


type t

val make_e_type: string -> Gamma.t -> t

val make_e_term: string -> Gamma.term -> t

val make_c_type: string -> c_hole -> Gamma.t -> t

val push_arg: bool -> int -> t -> t

val count_args: t -> int

val arg: int -> t -> bool * int

val args: t -> (bool * int) array

val signature: t -> (bool * int) array * Gamma.term

val count_implicits: int -> t -> int

val is_unifiable: t -> bool

val get_range: t -> Fmlib_parse.Position.range

val c_hole_info: t -> c_hole

val type_of: t -> Gamma.term

val doc: t -> Fmlib_pretty.Print.doc
