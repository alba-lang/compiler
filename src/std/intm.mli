val for_all: (int -> bool) -> int -> bool


module Make (M: Fmlib_std.Interfaces.MONAD):
sig
    val fold_up:   (int -> 'a -> 'a M.t) -> 'a -> int  -> 'a M.t

    val fold_down: (int -> 'a -> 'a M.t) -> 'a -> int  -> 'a M.t
end
