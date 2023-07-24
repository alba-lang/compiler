module Make (Key: Fmlib_std.Interfaces.SORTABLE):

sig
    type 'a t

    val empty: 'a t

    val add: Key.t -> 'a -> 'a t -> 'a t

    val find: Key.t -> 'a t -> 'a list
end
