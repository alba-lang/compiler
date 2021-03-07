open Fmlib_parse

include Interfaces.LEXER
    with type final = Position.range * Token.t
