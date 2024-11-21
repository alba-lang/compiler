open Fmlib_parse
open Std


module Parser:
sig
    include Interfaces.FULL_PARSER
        with type state    = Elab.State.t
         and type token    = Char.t
         and type expect   = string * Indent.expectation option
         and type final    = Elab.Final.t
         and type semantic = Error.t


    val position: t -> Position.t
    val run_on_string: string -> t -> t
end


val term_parser: Elab.State.t -> Parser.t
