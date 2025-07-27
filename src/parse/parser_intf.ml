open Fmlib_parse
open Std

module type ELABORATOR =
sig
    type t

    type term
    type src_term

    val prop: Position.range -> src_term
    val any:  Position.range -> src_term

    val make_term: src_term -> t -> (term, Error.t) result
end
