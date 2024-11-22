open Std
open Fmlib_pretty

include Generic_elaborator.TRACER
    with type message = unit -> Print.doc

val empty: t

val ignore: t

val doc: t -> Print.doc
