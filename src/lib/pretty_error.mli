open Fmlib_pretty

type doc       = Print.doc
type doc_thunk = unit -> doc

val of_doc: doc -> doc_thunk

val of_thunk: doc_thunk -> doc_thunk

val of_text: string -> doc_thunk

val paragraph: doc list -> doc

val unexpected: string -> doc_thunk

val no_root_found: string -> doc_thunk

val already_project: string -> string -> doc_thunk

val nested_project: string -> string -> doc_thunk

val cannot_do1: string -> string -> doc_thunk

val cannot_do2: string -> string -> doc_thunk

val expecting_one_of: string list -> doc_thunk
