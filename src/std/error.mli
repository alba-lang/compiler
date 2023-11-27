open Fmlib_parse
open Fmlib_pretty

type t

val make: Position.range -> string -> (unit -> Print.doc) -> t

val range: t -> Position.range

val line1: t -> int
val line2: t -> int

val column1: t -> int
val column2: t -> int

val tag:   t -> string

val doc:   t -> Print.doc


val cannot_infer_type: Position.range -> t
