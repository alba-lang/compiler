type doc = Fmlib_pretty.Print.doc


module Term:
sig
    val base_doc: bool -> bool -> Term.t -> doc
    val doc:      Term.t -> doc

    val base_string: bool -> bool -> Term.t -> string
    val string:   Term.t -> string
end
