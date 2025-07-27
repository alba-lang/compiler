open Fmlib_parse

open Std
open Parser_intf







module Term (E: ELABORATOR):
sig
    module Parser:
    sig
        include Interfaces.FULL_PARSER
            with type state    := E.t
             and type token    := Position.range * Token.t
             and type final    := E.term
             and type expect   := string * Indent.expectation option
             and type semantic := Error.t
    end

    val make:         E.t -> Parser.t
    val make_partial: E.t -> Parser.t
end
