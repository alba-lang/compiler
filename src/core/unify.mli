open Base_elab

type range = Fmlib_parse.Position.range

module Make (Final: ANY):
sig
    open Base_elab.Make (Final)

    val two: range -> term -> term -> term t
    val into_hole: range -> term -> int -> term t
end
