open Fmlib_parse

type doc         = Fmlib_pretty.Print.doc

type range_thunk = unit -> Position.range
type doc_thunk   = unit -> doc



module Package:
sig
    type t
end

module Parser:
sig
    include Fmlib_parse.Interfaces.NORMAL_PARSER
        with type token    = char
         and type final    = Package.t
         and type expect   = string * Indent.expectation option
         and type semantic = range_thunk * doc_thunk

    val init: t

    val position: t -> Position.t

    val range: semantic -> Position.range

    val doc: semantic -> doc
end
