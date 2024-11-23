type doc = Fmlib_pretty.Print.doc


module Term:
sig
    val doc:      Term.t -> doc
    val full_doc: Term.t -> doc

    val string:   Term.t -> string
end
