type range = Fmlib_parse.Position.range


type t

val make_e_type: range -> string -> Gamma.t -> t

val make_e_term: range -> string -> Gamma.term -> t

val make_c_variable_type:   range -> string -> Gamma.t -> t

val make_c_expression_type: range -> string -> Gamma.t -> t

val push_arg: bool -> int -> t -> t

val count_args: t -> int

val arg: int -> t -> bool * int

val args: t -> (bool * int) array

val signature: t -> (bool * int) array * Gamma.term

val count_implicits: int -> t -> int

val is_unifiable: t -> bool

val get_range: t -> Fmlib_parse.Position.range

val type_of: t -> Gamma.term

val trace_doc: t -> Fmlib_pretty.Print.doc

val error: int -> t -> Std.Error.t
