open Fmlib_pretty.Print

val indent: doc -> doc

val description: doc -> doc -> doc

val descriptions: (doc * doc) list -> doc
