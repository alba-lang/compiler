open Alba_parse

module TP = Definition_parser.Make (Elaborator.Semantic) (Elaborator)

module PL =
    Fmlib_parse.Parse_with_lexer.Make
        (Elaborator)
        (Token)
        (Unit)
        (Definition_parser.Error (Elaborator.Semantic) (Elaborator))
        (Lexer)
        (TP)

let lexer: Lexer.t = Lexer.start

let token_parser: TP.t =
    TP.make (Elaborator.init "alba.core" "basic")

let parser: PL.t =
    PL.make lexer token_parser
