module Semantic:
sig
    type t
end



include Alba_parse.Definition_parser.ELABORATOR
    with type error = Semantic.t


val init: string -> string -> t

val environment: t -> Environment.t
