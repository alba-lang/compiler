open Base_elab


module Make (Final: ANY):
sig
    open Base_elab.Make (Final)

    val two: term -> term -> term t
    val into_hole: term -> int -> term t
end
