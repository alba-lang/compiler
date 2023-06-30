open Core

type range = Fmlib_parse.Position.range

type t

let make (_: unit -> range) (_: Term.t) (_: Term.t): t =
    assert false
