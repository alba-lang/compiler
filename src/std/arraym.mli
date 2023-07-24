module Make (M: Fmlib_std.Interfaces.MONAD):
sig
    val mapi: (int -> 'a -> 'b M.t) -> 'a array -> 'b array M.t
end
