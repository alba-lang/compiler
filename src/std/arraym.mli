module Make (M: Fmlib_std.Interfaces.MONAD):
sig
    val mapi: (int -> 'a -> 'b M.t) -> 'a array -> 'b array M.t


    val foldi_left:
        ('accu -> int -> 'el -> 'accu M.t)
        -> 'accu
        -> 'el array
        -> 'accu M.t


    val fold_left:
        ('accu -> 'el -> 'accu M.t)
        -> 'accu
        -> 'el array
        -> 'accu M.t
end
